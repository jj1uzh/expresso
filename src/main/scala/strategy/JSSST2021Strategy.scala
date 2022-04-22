package com.github.kmn4.expresso.strategy

import com.github.kmn4.expresso._
import com.github.kmn4.expresso.math._
import com.github.kmn4.expresso.machine._
import com.github.kmn4.expresso.language._
import com.github.kmn4.expresso.language.Constraint._
import com.typesafe.scalalogging.Logger
import collection.mutable.{Map => MMap}

class JSSST2021Strategy(implicit logger: Logger) extends Strategy {
  private type SolverPSST[C, I] = ParikhSST[Int, Option[C], Option[C], Int, Int, I]
  private type SolverPA[C, I] = ParikhAutomaton[Int, Option[C], Int, I]
  private type SolverPA2[C, I] = ParikhAutomaton[(Int, Int), Option[C], Cop[Int, Int], I]

  private type Memo[A] = MMap[Unit, A]

  private implicit class MemoOps[A](m: Memo[A]) {
    def shouldGet: A = m(())
    def getOr(f: => A) = m.getOrElseUpdate((), f)
  }

  private val sat: Memo[Boolean] = MMap.empty
  override def issat = sat(())

  private val witnessVector: Memo[Option[(Map[String, Int], Map[Int, Int])]] = MMap.empty

  private val pa: Memo[SolverPA[Char, String]] = MMap.empty

  private val transductions: Memo[Map[String, Int] => Seq[Option[Char]] => Set[Seq[Option[Char]]]] =
    MMap.empty

  private val models: Memo[Output] = MMap.empty

  override def checkSat(constraint: Input): Boolean =
    sat.getOr {
      val Input(alphabet, stringVarNumber, assignments, assertions, arithFormulas, _) = constraint
      val maxVar = stringVarNumber - 1
      pa(()) = {
        val asgnPSSTs = assignments.map(a => assignmentToPSST(a, alphabet))
        val idxLangs = assertions.groupMap(_.stringVar)(_.lang)
        val lastPA = {
          val p = compileParikhAssertions(idxLangs, alphabet, maxVar)
          val is = arithFormulas.flatMap(_.freeVars)
          val formulas = arithFormulas.map(_.renameVars[Either[String, Int]](Left.apply))
          val pa1 = p.copy(is = p.is ++ is, acceptFormulas = p.acceptFormulas ++ formulas)
          pa1
        }
        logger.trace("got the following PSSTs:")
        asgnPSSTs.zipWithIndex.foreach { case (psst, i) =>
          logger.trace(s"#$i: ${psst.sizes}")
        }
        transductions(()) = v => {
          type S = Seq[Option[Char]]
          val compose = (f: S => Set[S], g: S => Set[S]) => (w: S) => f(w).flatMap(g)
          asgnPSSTs.map(p => (w: S) => p.transduce(w, v)).foldLeft[S => Set[S]](Set(_))(compose)
        }
        val res = asgnPSSTs.foldRight(lastPA) { case (acc, p) =>
          logger.info(s"compose ${acc.sizes} and ${p.sizes}")
          (acc preimage p).renamed
        }
        logger.info(s"composition done: ${res.sizes}")
        res
      }
      witnessVector.getOr(pa(()).ilVectorOption).nonEmpty
    }

  override def getModel(): Output =
    models.getOr {
      val p = pa.shouldGet
      witnessVector.shouldGet.map { case (iv, lv) =>
        val (in, _) = p.psst.inputOutputFor(lv)
        val output = transductions.shouldGet(iv)(in).head
        val ss = parseStrModel(output)
        (ss, iv)
      }
    }

  override def checkSatForall(v: String, constraint: Input): Boolean = {
    val Input(alphabet, stringVarNumber, assignments, assertions, arithFormulas, _) = constraint
    val maxVar = stringVarNumber - 1
    pa(()) = {
      val asgnPSSTs = assignments.map(a => assignmentToPSST(a, alphabet))
      val idxLangs = assertions.groupMap(_.stringVar)(_.lang)
      val lastPA = {
        val p = compileParikhAssertions(idxLangs, alphabet, maxVar)
        val is = arithFormulas.flatMap(_.freeVars)
        val formulas = arithFormulas.map(_.renameVars[Either[String, Int]](Left.apply))
        p.copy(is = p.is ++ is, acceptFormulas = p.acceptFormulas ++ formulas)
      }
      logger.trace("got the following PSSTs:")
      asgnPSSTs.zipWithIndex.foreach { case (psst, i) =>
        logger.trace(s"#$i: ${psst.sizes}")
      }
      transductions(()) = v => {
        type S = Seq[Option[Char]]
        val compose = (f: S => Set[S], g: S => Set[S]) => (w: S) => f(w).flatMap(g)
        asgnPSSTs.map(p => (w: S) => p.transduce(w, v)).foldLeft[S => Set[S]](Set(_))(compose)
      }
      val res = asgnPSSTs.foldRight(lastPA) { case (acc, p) =>
        logger.info(s"compose ${acc.sizes} and ${p.sizes}")
        (acc preimage p).renamed
      }
      logger.info(s"composition done: ${res.sizes}")
      res
    }
    pa(()).isSatForall(v)
  }

  private def parseStrModel(output: Seq[Option[Char]]): Seq[String] = {
    var buf = output
    var idx = 0
    var res = Seq.empty[String]
    while (buf.nonEmpty) {
      val took = buf.takeWhile(_.nonEmpty).flatten.mkString
      buf = buf.drop(took.length + 1)
      res :+= took
      idx += 1
    }
    res
  }

  // y = f(x) の形について，f が unary な場合と連接の場合とがある．
  private def transToSolverPSST[C, I](
      t: ParikhTransduction[C, I],
      alphabet: Set[C],
      lhsStringVarIdx: Int,
      rhsStringVarIdx: Int
  ): SolverPSST[C, I] = {
    sealed trait X
    case object XIn extends X
    case class XJ(x: Int) extends X
    type Q = (Int, Int)
    type A = Option[C]
    type UpdateX = Update[X, A]
    type UpdateL = Map[Int, Int]
    type Edges = Iterable[(Q, A, UpdateX, UpdateL, Q)]
    val j = rhsStringVarIdx
    val jsst = t.toParikhSST(alphabet)
    val xjs: Set[X] = jsst.xs.map(XJ.apply)
    val xj = xjs.head
    val base =
      solverNsstTemplate[C, X](
        lhsStringVarIdx,
        alphabet,
        XIn,
        xjs,
        List(Cop1(XIn), Cop1(xj), Cop2(None))
      ).toParikhSST[Int, I](jsst.ls)
    val xs = base.xs
    val updates: Monoid[UpdateX] = updateMonoid(xs)
    val states: Set[Q] = base.states - ((j, 0)) ++ jsst.states.map((j, _))
    val edges: Edges = {
      val baseNoJ = base.edges.filter { case (q, a, m, v, r) =>
        (q._1 != j) && (r._1 != j)
      }
      def unit(a: A): UpdateX = updates.unit + (XIn -> List(Cop1(XIn), Cop2(a)))
      def reset(a: A): UpdateX = xs.map(_ -> Nil).toMap + (XIn -> List(Cop1(XIn), Cop2(a)))
      val toJ =
        ((j - 1, 0), None, unit(None), jsst.ls.map(_ -> 0).toMap, (j, jsst.q0))
      def embedList(l: Cupstar[Int, C]): Cupstar[X, A] = l.map(_.map1(XJ.apply)).map(_.map2(Option.apply))
      def embedUpdate(m: Update[Int, C]): Update[X, A] = m.map { case (x, l) => XJ(x) -> embedList(l) }
      val withinJ: Edges = jsst.edges.map { case (q, a, m, v, r) =>
        (((j, q), Some(a), embedUpdate(m) + (XIn -> List(Cop1(XIn), Cop2(Some(a)))), v, (j, r)))
      }
      val fromJ: Edges =
        for ((qf, s) <- jsst.outF; (l, v) <- s)
          yield ((j, qf), None, reset(None) + (xj -> embedList(l)), v, (j + 1, 0))

      baseNoJ + toJ ++ withinJ ++ fromJ
    }

    ParikhSST[Q, A, A, X, Int, I](
      states,
      base.inSet,
      xs ++ xjs,
      jsst.ls,
      jsst.is,
      edges.toSet,
      if (j == 0) (j, jsst.q0) else (0, 0),
      base.outGraph,
      jsst.acceptFormulas
    ).renamed
  }

  private def assignmentToPSST(
      assignment: AtomicAssignment[Int],
      alphabet: Set[Char]
  ): SolverPSST[Char, String] = assignment match {
    case ParikhAssignment(lhsStringVar, trans, rhsStringVar) =>
      transToSolverPSST(trans, alphabet, lhsStringVar, rhsStringVar)
    case CatAssignment(lhsStringVar, wordAndVars) =>
      concatNSST(lhsStringVar, wordAndVars, alphabet).toParikhSST
    case InsertAssignment(_, _, _, _) => throw new Exception("Not Implemented")
  }

  /** Returns `alphabet` to `alphabet` NSST whose state set is {(0, 0), ... (n, 0)} and variable set is
    * `inputVariable +: otherVariables`. On each state (i, 0) the NSST appends input character to
    * `inputVariable`, and identity on `otherVariables`. It transitions to next state when it reads `None`,
    * appending `None` to `inputVariable`. Its output function value will be `Set(output)` on state (n, 0),
    * and empty on other ones. So the NSST reads string of the form "w0 None w1 None ... w(n-1) None" and
    * outputs `output` where `inputVariable` is replaced with "w0 None ... w(n-1) None".
    */
  private def solverNsstTemplate[C, X](
      n: Int,
      alphabet: Set[C],
      inputVariable: X,
      otherVariables: Set[X],
      output: List[Cop[X, Option[C]]]
  ): NSST[(Int, Int), Option[C], Option[C], X] = {
    type Q = (Int, Int)
    type A = Option[C]
    type B = Option[C]
    val states = Set.from(for (i <- 0 to n) yield (i, 0))
    val inSet = (alphabet.map[Option[C]](Some(_))) + None
    val xs = otherVariables + inputVariable
    val outF: Map[Q, Set[Cupstar[X, B]]] = Map((n, 0) -> Set(output))
    val updates = updateMonoid(xs)
    type Edges = Set[(Q, A, Update[X, B], Q)]
    val edges: Edges =
      for ((i, _) <- states; a <- inSet if i != n)
        yield (
          (i, 0),
          a,
          updates.unit + (inputVariable -> List(Cop1(inputVariable), Cop2(a))),
          (if (a == None) i + 1 else i, 0)
        )
    NSST(states, inSet, xs, edges, (0, 0), outF)
  }

  /** x(i) := word */
  def constantNSST[C](i: Int, word: Seq[C], alphabet: Set[C]): SolverSST[C] = {
    solverNsstTemplate(
      i,
      alphabet,
      (),
      Set.empty,
      List(Cop1(())) ++ word.map(a => Cop2(Some(a))) ++ List(Cop2(None))
    ).renamed
  }

  /** Construct NSST which output concatenation of `rhs`. Right(j) in `rhs` is `j`-th input delemited by #.
    */
  private def concatNSST[C](i: Int, rhs: Seq[Either[Seq[C], Int]], alphabet: Set[C]): SolverSST[C] = {
    trait X
    case object XIn extends X
    case class XJ(j: Int, id: Int) extends X
    val concated = rhs.zipWithIndex
      .flatMap[Cop[X, Option[C]]] {
        case (Left(as), _) => as.map(a => Cop2(Some(a)))
        case (Right(j), k) => Seq(Cop1(XJ(j, k)))
      }
      .toList
    val vars = concated.flatMap { case Cop1(x) => Some(x); case _ => None }
    val base =
      solverNsstTemplate(i, alphabet, XIn, vars.toSet, List(Cop1(XIn)) ++ concated ++ List(Cop2(None)))
    val edges = base.edges.map {
      case t @ ((q, 0), Some(a), m, (_, 0)) =>
        t.copy(_3 =
          m ++ vars
            .withFilter { case XJ(j, _) => j == q; case _ => false }
            .map(x => x -> List(Cop1(x), Cop2(Some(a))))
        )
      case other => other
    }
    base.copy(edges = edges).renamed
  }

  type SolverSST[A] = NSST[Int, Option[A], Option[A], Int]

  /** Returns ParikhAutomaton that accepts an input iff it meets constriant given by `pas`. That is, it reads
    * an input of the form w0#w1#...w(n-1)# (where n = dfas.length and # = None) and accepts if pas(i) accepts
    * w(i) for all i.
    */
  private def solverPA[Q, A, L, I](
      pas: Seq[ParikhAutomaton[Q, A, L, I]], // ordered by corresponding string variables.
      q: Q // this will be used as "default state", and any value of type Q will suffice.
  ): ParikhAutomaton[(Int, Q), Option[A], (Int, L), I] = {
    type NQ = (Int, Q) // Represents PA number by Int.
    type NA = Option[A]
    type NL = (Int, L)
    type UpdateL = Map[L, Int]
    type UpdateNL = Map[NL, Int]
    val ls = for {
      (pa, i) <- pas.zipWithIndex
      l <- pa.ls
    } yield (i, l)
    def update(v: UpdateL, i: Int): UpdateNL =
      ls.map {
        case nl @ (j, l) if j == i => nl -> v(l)
        case nl                    => nl -> 0
      }.toMap
    val initialState = (0, pas.headOption.map(_.q0).getOrElse(q))
    var states: Set[NQ] = Set.empty
    var edges: List[(NQ, NA, UpdateNL, NQ)] = Nil
    var acceptRelation: Set[(NQ, UpdateNL)] = Set.empty
    for ((pa, i) <- pas.zipWithIndex) {
      states ++= pa.states.map((i, _))
      edges ++:= acceptRelation.map { case (q, v) => (q, None, v, (i, pa.q0)) }
      edges ++:= pa.edges
        .map[(NQ, NA, UpdateNL, NQ)] { case (q, a, v, r) => ((i, q), Some(a), update(v, i), (i, r)) }
        .toList
      acceptRelation = pa.acceptRelation.map { case (q, v) => ((i, q), update(v, i)) }
    }
    val qf = (pas.length, q)
    states += qf
    edges ++= acceptRelation.map { case (q, v) => (q, None, v, qf) }
    val acceptFormulas = for {
      (pa, i) <- pas.zipWithIndex
      f <- pa.acceptFormulas
    } yield f.renameVars(_.map((i, _)))
    ParikhAutomaton[NQ, NA, NL, I](
      states,
      addNone(pas.flatMap(_.inSet).toSet),
      ls.toSet,
      pas.flatMap(_.is).toSet,
      edges.toSet,
      initialState,
      Set((qf, ls.map(_ -> 0).toMap)),
      acceptFormulas
    )
  }

  private def compileParikhAssertions(
      assertions: Map[Int, Seq[ParikhLanguage[Char, String]]],
      alphabet: Set[Char],
      lastVarIdx: Int
  ): ParikhAutomaton[Int, Option[Char], Int, String] = {
    require(
      assertions.isEmpty || lastVarIdx >= assertions.keys.max,
      "All LHS of PA assertions should be less than or equal to max LHS of assignments."
    )
    val idxRegularsParikhs = {
      assertions.map { case (i, langs) =>
        val rs = langs.collect { case ParikhLanguage.FromRegExp(re) => re }
        val ps = langs.filterNot(_.isInstanceOf[ParikhLanguage.FromRegExp[Char, String]])
        i -> (rs, ps)
      }
    }
    val idxPA = idxRegularsParikhs.view.mapValues { case (rs, ps) =>
      val dfas = rs.map(_.toNFA(alphabet).toDFA.minimized)
      val dfa = dfas
        .fold[DFA[Int, Char]](DFA.universal(0, alphabet)) { case (d1, d2) => (d1 intersect d2).renamed }
        .minimized
      val pa = ps
        .map(_.toParikhAutomaton(alphabet))
        .fold[ParikhAutomaton[Int, Char, Int, String]](ParikhAutomaton.universal(0, alphabet)) {
          case (p1, p2) => (p1 intersect p2).renamed
        }
      (dfa.toParikhAutomaton intersect pa).renamed
    }
    // (i, j) -- state j of a PSST of i-th string variable
    val universalPA = ParikhAutomaton.universal[Int, Char, Int, String](0, alphabet)
    solverPA((0 to lastVarIdx).map(idxPA.getOrElse(_, universalPA)), 0).renamed
  }
}
