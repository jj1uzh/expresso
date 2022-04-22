package com.github.kmn4.expresso.machine

import com.microsoft.z3

import com.github.kmn4.expresso._
import com.github.kmn4.expresso.machine.ParikhSST.Implicits._
import com.github.kmn4.expresso.math._

trait StringIntTransducer[A, B, I] {
  def transduce(w: Seq[A], n: Map[I, Int]): Set[Seq[B]]
}

/**
  * Parikh SST.
  *
  * `ls` should not appear as bound variables in acceptFormulas.
  */
case class ParikhSST[Q, A, B, X, L, I](
    states: Set[Q],
    inSet: Set[A],
    xs: Set[X],
    ls: Set[L],
    is: Set[I],
    edges: Set[(Q, A, Update[X, B], ParikhSST.ParikhUpdate[L], Q)],
    q0: Q,
    outGraph: Set[(Q, Cupstar[X, B], Map[L, Int])],
    acceptFormulas: Seq[Presburger.Formula[Either[I, L]]]
) extends StringIntTransducer[A, B, I] {
  type XBS = Cupstar[X, B]
  type LVal = Map[L, Int]
  type UpdateX = Update[X, B]
  type UpdateL = ParikhSST.ParikhUpdate[L]
  type UpdateXL = (UpdateX, UpdateL)
  val trans: Map[(Q, A), Set[(Q, UpdateXL)]] = graphToMap(edges) {
    case (q, a, mx, ml, r) => (q, a) -> (r, (mx, ml))
  }
  val outF: Map[Q, Set[(XBS, LVal)]] = graphToMap(outGraph) { case (q, xbs, lv) => q -> (xbs, lv) }
  val acceptFormula: Presburger.Formula[Either[I, L]] = Presburger.Conj(acceptFormulas.distinct)
  def evalFormula(lVal: Map[L, Int], iVal: Map[I, Int]): Boolean = acceptFormula.eval {
    val l = lVal.map { case (l, n) => Right(l) -> n }
    val i = iVal.map { case (i, n) => Left(i) -> n }
    (l ++ i).toMap
  }

  lazy val sst: NSST[Q, A, B, X] = NSST(
    states,
    inSet,
    xs,
    edges.map { case (q, a, mx, ml, r) => (q, a, mx, r) },
    q0,
    graphToMap(outGraph) { case (q, xbs, _) => q -> xbs }
  )
  private val mxMonoid: Monoid[UpdateX] = updateMonoid(xs)
  private val mlMonoid: Monoid[UpdateL] = Monoid.vectorMonoid(ls)
  private val mxlMonoid: Monoid[UpdateXL] = Monoid.productMonoid(mxMonoid, mlMonoid)

  def transition(qs: Set[Q], w: Seq[A]): Set[(Q, UpdateXL)] =
    Monoid.transition(qs, w.toList, (q: Q, a: A) => trans(q, a))(mxlMonoid)
  def outputAt(q: Q, m: UpdateXL, n: Map[I, Int]): Set[List[B]] = {
    val (mx, ml) = m
    outF(q).flatMap {
      case (xbs, lv) =>
        val lMap = mlMonoid.combine(ml, lv)
        if (evalFormula(lMap, n)) Some(erase1(flatMap1(xbs, mx)))
        else None
    }
  }
  def transduce(w: Seq[A], n: Map[I, Int]): Set[Seq[B]] = transition(Set(q0), w).flatMap {
    case (q, m) => outputAt(q, m, n)
  }
  def transduce(w: Seq[A]): Set[(List[B], Map[L, Int])] = transition(Set(q0), w).flatMap {
    case (q, (mx, ml)) => outF(q).map { case (xbs, lv) => (mx.update(emptyEnv).eval(xbs), ml(lv)) }
  }

  def renamed[R, Y, K](
      stateMap: Q => R = identity _,
      xMap: X => Y = identity _,
      lMap: L => K = identity _
  ): ParikhSST[R, A, B, Y, K, I] = {
    def renameXbs(xbs: XBS): Cupstar[Y, B] = xbs.map(_.map1(xMap))
    def renameLVal(lv: LVal): Map[K, Int] = lv.map { case (l, n) => lMap(l) -> n }
    val newEdges =
      edges
        .flatMap {
          case (q, a, mx, ml, r) if states.contains(q) && states.contains(r) =>
            Some(
              (
                stateMap(q),
                a,
                mx.map { case (x, xbs) => xMap(x) -> renameXbs(xbs) },
                renameLVal(ml),
                stateMap(r)
              )
            )
          case _ => None
        }
    val newF = outGraph.withFilter { case (q, _, _) => states(q) }.map {
      case (q, xbs, lv) => (stateMap(q), renameXbs(xbs), renameLVal(lv))
    }
    ParikhSST(
      states.map(stateMap),
      inSet,
      xs.map(xMap),
      ls.map(lMap),
      is,
      newEdges,
      stateMap(q0),
      newF,
      acceptFormulas.map(_.renameVars(l => l.map(lMap)))
    )
  }

  def renamed: ParikhSST[Int, A, B, Int, Int, I] = {
    val stateMap = (states.zipWithIndex).toMap
    val xMap = (xs.zipWithIndex).toMap
    val lMap = (ls.zipWithIndex).toMap
    renamed(stateMap, xMap, lMap)
  }

  def sizes: (Int, Int, Int, Int, Int, Int) =
    (states.size, xs.size, ls.size, edges.size, outGraph.size, acceptFormulas.size)

  private lazy val nonEmptyVarsAt: Map[Q, Set[X]] = {
    import scala.collection.mutable.{Map => MMap, Set => MSet}
    val res: MMap[Q, MSet[X]] = MMap.empty.withDefault(_ => MSet.empty)
    def charExistsIn(xbs: XBS): Boolean = xbs.exists(_.is2)
    var updated = false
    do {
      updated = false
      for ((q, _, mx, ml, r) <- edges) {
        val charAssignedX = xs.filter(x => charExistsIn(mx(x)))
        val nonEmptyXAssigned = xs.filter(x => varsIn(mx(x)).exists(res(q).contains))
        val addX = charAssignedX ++ nonEmptyXAssigned
        val resX = res(r)
        if (!(addX subsetOf resX)) {
          updated = true
          resX.addAll(addX)
          res(r) = resX
        }
      }
    } while (updated)

    Map.from { res.map { case (q, s) => q -> s.toSet } }.withDefaultValue(Set.empty)
  }

  /** Returns a PSST with redundant variables removed. */
  private def removeRedundantVars: ParikhSST[Q, A, B, X, L, I] = {
    val newVars = states.flatMap(nonEmptyVarsAt)
    def deleteNotUsed(alpha: XBS): XBS =
      alpha.filter { case Cop1(x) => newVars contains x; case _ => true }
    def newUpdate(m: Update[X, B]): Update[X, B] =
      newVars.map(x => x -> deleteNotUsed(m(x))).toMap
    val newEdges =
      edges.map { case (q, a, mx, ml, r)               => (q, a, newUpdate(mx), ml, r) }
    val newOutGraph = outGraph.map { case (q, xbs, lv) => (q, deleteNotUsed(xbs), lv) }
    this.copy(xs = newVars, edges = newEdges, outGraph = newOutGraph)
  }

  private def removeRedundantLogVars: ParikhSST[Q, A, B, X, L, I] = {
    val zeroVars = ls.filter(l =>
      edges.forall { case (q, a, m, v, r) => v(l) == 0 } && outGraph.forall { case (q, m, v) => v(l) == 0 }
    )
    copy(
      ls = ls -- zeroVars,
      edges = edges.map { case e @ (_, _, _, v, _) => e.copy(_4 = v.removedAll(zeroVars)) },
      outGraph = outGraph.map { case r @ (_, _, v) => r.copy(_3 = v.removedAll(zeroVars)) },
      acceptFormulas = acceptFormulas.map(Presburger.Formula.substitute(_) {
        case Left(i)                 => Presburger.Var(Left(i))
        case Right(l) if zeroVars(l) => Presburger.Const(0)
        case Right(l)                => Presburger.Var(Right(l))
      })
    )
  }

  // Think of product construction of two NSSTs.
  // invTransX(p)(r, x) is a set of state `q`s where q may transition to r by reading
  // a content of x at state p.
  private def invTransX[R, M](
      thatStates: Set[R],
      thatTrans: (R, B) => Set[(R, M)]
  ): Map[Q, Map[(R, X), Set[R]]] = {
    import scala.collection.mutable.{Map => MMap, Set => MSet}
    // At n1.q0, all x can contain empty string, thus q2 can transition to q2.
    // At other q1s, nothing is known about content of each x, hence empty destination set.
    val res: MMap[(Q, X, R), MSet[R]] =
      MMap.empty.withDefault {
        case (q1, x, q2) => (if (q1 == q0) MSet(q2) else MSet.empty)
      }
    def trans(transX: (R, X) => MSet[R])(q: R, xb: Cop[X, B]): Set[(R, Unit)] =
      xb match {
        case Cop1(x) => Set.from(transX(q, x).map((_, ())))
        case Cop2(b) => thatTrans(q, b).map { case (r, _) => (r, ()) }
      }
    // q1 の xbs で q2 から到達可能な状態集合
    def transition(q1: Q, xbs: XBS, q2: R): Set[R] =
      Monoid.transition(Set(q2), xbs, trans { case (q, y) => res((q1, y, q)) }).map(_._1)
    var updated = true
    while (updated) {
      updated = false
      // q1 =[???/m]=> r1, q2 =[(m(x)@q1)/???]=> r2 then q2 =[(x@r1)/???]=> r2
      for {
        (q1, _, mx, ml, r1) <- edges
        x <- xs
        q2 <- thatStates
      } {
        val cur = res((r1, x, q2))
        val added = transition(q1, mx(x), q2)
        if (!(added subsetOf cur)) {
          updated = true
          cur ++= added
        }
        res((r1, x, q2)) = cur
      }
    }

    (for (((q1, x, q2), s) <- res; r2 <- s) yield (q1, x, q2, r2))
      .groupMap(_._1) { case (_, x, q2, r2) => (x, q2, r2) }
      .view
      .mapValues(graphToMap(_) { case (x, q2, r2) => (r2, x) -> q2 })
      .toMap
      .withDefaultValue(Map.empty.withDefault { case (q2, _) => Set(q2) })
  }

  // xbs@q を読んで r1 から r2 にいけるような推測と効果
  private def possiblePreviousGuess[R, M](
      invTransB: Map[(R, B), Set[(R, M)]],
      // x@q で r1 から r2 に行ける ⇒ r1 ∈ invTransX(q)(r2, x)
      invTransX: Map[Q, Map[(R, X), Set[R]]]
  )(q: Q, r1: R, r2: R, xbs: Cupstar[X, B]): Set[(Map[X, (R, R)], Cupstar[X, M])] = {
    val invTransXAtQ = invTransX(q)
    xbs
      .foldRight[Set[(Map[X, (R, R)], (R, Cupstar[X, M]))]](
        Set((Map.empty, (r2, Nil))) // accumulates a set of pairs of a mapping and configuration.
      ) {
        case (Cop1(x), acc) =>
          acc.flatMap {
            case (m, (r, xbs)) =>
              invTransXAtQ(r, x).map(q => (m + (x -> (q, r)), (q, Cop1(x) :: xbs)))
          }
        case (Cop2(a), acc) =>
          acc.flatMap {
            case (m, (r, xbs)) =>
              invTransB(r, a).map { case (q, b) => (m, (q, Cop2(b) :: xbs)) }
          }
      }
      .withFilter { case (_, (s, _)) => s == r1 }
      .map { case (m, (_, xbs)) => (m, xbs) }
  }

  private def andThenAux[R, C, Y, K](
      that: ParikhSST[R, B, C, Y, K, I]
  ) = {
    // (PSST, PSST) => (Update[Y, C], Update[Y, V[K]])-PSST => Update[Y, C]-PSST
    //// 中間的な状態やラベルの型
    type V[T] = Map[T, Int]
    type WithVL[T] = (T, V[L])
    type Q1 = (Q, Map[X, (R, R)]) // guess how `that` transitions on each x
    type Q2 = (Q1, Set[X]) // guess which Xs contribute to V[K] output

    implicit val vkMonoid: Monoid[V[K]] = Monoid.vectorMonoid(that.ls)

    //// MPA への変換
    val invTransA: Map[(Q, A), Set[(Q, UpdateXL)]] =
      graphToMap(edges) { case (q, a, mx, ml, r) => (r, a) -> ((q, (mx, ml))) }

    val invTransB: Map[(R, B), Set[(R, that.UpdateXL)]] =
      graphToMap(that.edges) { case (q, b, ycs, kv, r) => (r, b) -> (q, (ycs, kv)) }

    val invTransX: Map[Q, Map[(R, X), Set[R]]] = this.invTransX(that.states, that.trans(_, _))
//    val invTransX: Map[Q, Map[(R, X), Set[R]]] = this.invTransX(that.states, that.trans(_, _))

    val previousGuess = this.possiblePreviousGuess(invTransB, invTransX) _
    val countAndFold: Cupstar[X, V[K]] => (Set[X], V[K]) = xvs => {
      val (xs, vs) = xvs.partitionMap(_.toEither)
      (xs.toSet, Monoid.fold(vs))
    }
    val countAndFoldUpdate: Update[X, V[K]] => X => (Set[X], V[K]) = update => x => countAndFold(update(x))

    // For each element (f, xbs) of the returned set, the following holds: q -[xas / xbs]-> r by using f.
    def previousStates(mq: Q1, a: A): Iterator[WithVL[(Q1, Update[X, that.UpdateXL])]] = {
      val (r, guess) = mq
      invTransA(r, a).iterator.flatMap {
        case (q, (mx, lv)) => // q -[a / m]-> r
          val candidates: List[(X, Set[(Map[X, (R, R)], Cupstar[X, that.UpdateXL])])] =
            guess.iterator.map {
              case (x, (r1, r2)) =>
                // Variables always empty at state q can be ignored
                val nonEmptyAtQ = nonEmptyVarsAt(q)
                val filtered: Cupstar[X, B] = mx(x).filter {
                  case Cop1(x) => nonEmptyAtQ(x)
                  case _       => true
                }
                x -> previousGuess(q, r1, r2, filtered)
            }.toList

          def aux(
              candidates: List[(X, Set[(Map[X, (R, R)], Cupstar[X, that.UpdateXL])])]
          ): Iterator[(Map[X, (R, R)], Update[X, that.UpdateXL])] =
            candidates match {
              case Nil => Iterator((Map.empty, xs.map(x => x -> Nil).toMap))
              case (x, s) :: tl =>
                for ((guess1, mu) <- aux(tl); (guess2, xms) <- s) yield ((guess1 ++ guess2), mu + (x -> xms))
            }

          aux(candidates).map { case (guess, newMx) => (((q, guess), newMx), lv) }
      }
    }

    val mvOutGraph: Iterator[
      WithVL[(Q1, (Cupstar[X, that.UpdateX], that.XBS), (Cupstar[X, that.UpdateL], that.LVal))]
    ] = for {
      (q, xbs, lv) <- outGraph.iterator
      (r, s2) <- that.outF.iterator
      (guess, xms) <- {
        val nonEmptyAtQ1 = nonEmptyVarsAt(q)
        val filtered = xbs.filter { case Cop1(x) => nonEmptyAtQ1(x); case _ => true }
        previousGuess(q, that.q0, r, filtered)
      }
      (ycs, kv) <- s2
    } yield {
      val (xuys, xvs) = Cupstar.devideChars(xms)
      (((q, guess), (xuys, ycs), (xvs, kv)), lv)
    }

    val mpsstOutIter: Iterator[WithVL[(Q2, (Cupstar[X, that.UpdateX], that.XBS), that.LVal)]] =
      mvOutGraph.map {
        case ((q, stringPart, (xvs, kv1)), lv) =>
          val (outXs, kv2) = countAndFold(xvs)
          val kv = Monoid.fold(Iterator(kv1, kv2))
          (((q, outXs), stringPart, kv), lv)
      }

    //// 2nd-MPSST への変換

    def mpsstPreviousStates(pr: Q2, a: A): IterableOnce[WithVL[(Q2, Update[X, that.UpdateX], V[K])]] = {
      val (r, rXs) = pr
      for {
        ((q, update), lv) <- previousStates(r, a)
      } yield {
        val (stringUpdate, vectorUpdate) = Update.devideChars(update)
        val iter = rXs.iterator.map(countAndFoldUpdate(vectorUpdate))
        val (qXs, kv) = iter.fold((Set.empty[X], that.ls.map(_ -> 0).toMap)) {
          case ((accXs, accV), (xs, v)) => (accXs | xs, Monoid[that.UpdateL].combine(accV, v))
        }
        countAndFoldUpdate(vectorUpdate)
        (((q, qXs), stringUpdate, kv), lv)
      }
    }

    def lks: Iterator[Either[L, K]] = this.ls.iterator.map(Left.apply) ++ that.ls.iterator.map(Right.apply)
    val lkRename: Map[Either[L, K], Int] = lks.zipWithIndex.toMap
    def combineLK(lv: this.LVal, kv: that.LVal): Map[Int, Int] =
      lks.map {
        case e @ Left(l)  => lkRename(e) -> lv(l)
        case e @ Right(k) => lkRename(e) -> kv(k)
      }.toMap
    val formulas = {
      val thisF = acceptFormulas.map(_.renameVars(_.map(l => lkRename(Left(l)))))
      val paF = that.acceptFormulas.map(_.renameVars(_.map(k => lkRename(Right(k)))))
      thisF ++ paF
    }

    (mpsstOutIter, mpsstPreviousStates _, lkRename.values.toSet, combineLK _, formulas)
  }

  def preimageIter[R, K](pa: ParikhAutomaton[R, B, K, I]): Iterator[ParikhAutomaton[Int, A, Int, I]] = {
    val that = pa.toParikhSST
    val intermediate = andThenAux(that)
    val (paOutGraph, paPreviousStates, lks, combineLK, formulas) = intermediate
    paOutGraph.flatMap { out =>
      val ((qf, _, kv), lv) = out
      val lkv = combineLK(lv, kv)

      val (states, edges) = searchStates(Set(qf), inSet)(paPreviousStates)(
        { case ((q, _, _), _)           => q },
        { case (r, a, ((q, _, kv), lv)) => (q, a, combineLK(lv, kv), r) }
      )

      val initialStates =
        states.withFilter {
          case ((q, guess), _) => q == q0 && guess.forall { case (_, (r1, r2)) => r1 == r2 }
        }

      initialStates.map { q0 =>
        // Remove all unreachable states.
        val reachables = closure(Set(q0), graphToMap(edges) {
          case (q, _, _, r) => q -> r
        })
        val newEdges = edges.filter { case (q, _, _, r) => reachables(q) && reachables(r) }
        ParikhAutomaton(
          reachables,
          inSet,
          lks,
          is ++ pa.is,
          newEdges,
          q0,
          Set((qf, lkv)),
          formulas
        ).renamed
      }

    }
  }

  def preimage[R, K](pa: ParikhAutomaton[R, B, K, I]): ParikhAutomaton[Int, A, Int, I] = {
    val that = pa.toParikhSST
    val psst = andThen[R, Nothing, Nothing, K](that)
    ParikhAutomaton(
      psst.states,
      psst.inSet,
      psst.ls,
      psst.is,
      psst.edges.map { case (q, a, _, v, r) => (q, a, v, r) },
      psst.q0,
      psst.outGraph.map { case (q, _, v) => (q, v) },
      psst.acceptFormulas
    )
  }

  def andThen[R, C, Y, K](
      that: ParikhSST[R, B, C, Y, K, I]
  ): ParikhSST[Int, A, C, (X, Y, Boolean), Int, I] = {
    val (mpsstOutIter, mpsstPreviousStates, lks, combineLK, formulas) = andThenAux(that)
    val og = mpsstOutIter.toSet
    val (newStates, newEdges) = searchStatesInt(og.map { case ((q, _, _), _) => q }, inSet)(mpsstPreviousStates)(
      { case ((q, _, _), _)                => q },
      { case (r, a, ((q, m, kv), lv), map) => (map(q), a, m, combineLK(lv, kv), map(r)) }
    )
    val newOutGraph = og.map {
      case ((q, stringPart, kv), lv) => (newStates(q), stringPart, combineLK(lv, kv))
    }

    val initialStates = for {
      (((q, guess), _), qi) <- newStates if q == q0 && guess.forall { case (_, (r1, r2)) => r1 == r2 }
    } yield qi

    val mpsst = MonoidParikhSST[Int, A, C, X, Y, Int, I](
      newStates.values.toSet,
      inSet,
      xs,
      that.xs,
      lks,
      is ++ that.is,
      newEdges.toSet,
      initialStates.toSet,
      newOutGraph,
      formulas
    )
    mpsst.toParikhSST
  }

  def andThenIter[R, C, Y, K](
      that: ParikhSST[R, B, C, Y, K, I]
  ): Iterator[ParikhSST[Int, A, C, Int, Int, I]] = {
    val (mpsstOutIter, mpsstPreviousStates, lks, combineLK, formulas) = andThenAux(that)
    // 2nd-MPSST => PSST
    // guess      : Map[Y, Seq[Y]]
    // state      : (Q2, guess)
    // edge label : (Update[X, C], that.UpdateL)
    // out label  : (Cupstar[X, C], that.LVal)

    mpsstOutIter.flatMap { out =>
      val ((qf, stringPart, kv), lv) = out
      val lkv = combineLK(lv, kv)

      val (states, edges) = searchStates(Set(qf), inSet)(mpsstPreviousStates)(
        { case ((q, _, _), _)           => q },
        { case (r, a, ((q, m, kv), lv)) => (q, a, m, combineLK(lv, kv), r) }
      )

      val initialStates =
        states.withFilter {
          case ((q, guess), _) => q == q0 && guess.forall { case (_, (r1, r2)) => r1 == r2 }
        }

      initialStates.map { q0 =>
        // 2nd-MPSST => PSST
        val mpsst = MonoidParikhSST(
          states,
          inSet,
          xs,
          that.xs,
          lks,
          is ++ that.is,
          edges,
          Set(q0),
          Set((qf, stringPart, lkv)),
          formulas
        )
        val psst = mpsst.toParikhSST
        psst.renamed
      }

    }
  }

  def compose[R, C, Y, K](that: ParikhSST[R, B, C, Y, K, I]): ParikhSST[Int, A, C, Int, Int, I] =
    andThen(that).renamed.removeRedundantVars

  def toLogVectorEpsNFT: ENFT[Option[Q], A, Map[L, Int]] = {
    implicit val mon: Monoid[ParikhSST.ParikhUpdate[L]] = Monoid.vectorMonoid(ls)
    ENFT(
      addNone(states),
      inSet,
      edges.map { case (q, a, m, v, r) => (Option(q), Option(a), v, Option(r)) } ++ outGraph.map {
        case (q, _, v)                 => (Option(q), None, v, None)
      },
      Some(q0),
      None
    )
  }

  /** Returns a formula that captures the Parikh image of log variables of this PSST. */
  def parikhImageFormula: Presburger.Formula[Either[Int, L]] = {
    val formula = Parikh.parikhEnftToPresburgerFormula(toLogVectorEpsNFT)

    var i = 0
    val eMap = collection.mutable.Map.empty[(Option[Q], Map[L, Int], Option[Q]), Int]
    val qMap = collection.mutable.Map.empty[Option[Q], Int]
    def newVar = {
      i += 1
      i
    }
    formula.renameVars {
      case Parikh.BNum(l)     => Right(l)
      case Parikh.EdgeNum(e)  => Left(eMap.getOrElseUpdate(e, newVar))
      case Parikh.Distance(q) => Left(qMap.getOrElseUpdate(q, newVar))
    }
  }

  /** Returns a pair (n, v) of I vector and L vector that meets the following if exists:
    * there exists w and w' such that this transduces w to (w', v) and formula(n, v) == true. */
  def ilVectorOption: Option[(Map[I, Int], Map[L, Int])] = {
    val formulas = {
      acceptFormulas.map(_.renameVars {
        case Left(i)  => s"int_$i"
        case Right(l) => s"log_$l"
      }) :+ parikhImageFormula.renameVars {
        case Left(i)  => s"bound_$i"
        case Right(l) => s"log_$l"
      }
    }
    withZ3Context { (ctx) =>
      val solver = ctx.mkSolver()
      val z3Exprs = formulas.map(Presburger.Formula.formulaToZ3Expr(ctx, Map.empty[String, z3.IntExpr], _))
      solver.add(z3Exprs: _*)
      val result = solver.check()
      if (result == z3.Status.SATISFIABLE) {
        val model = solver.getModel()
        val n = is.map(i => i -> model.eval(ctx.mkIntConst(s"int_$i"), false).toString.toInt)
        val v = ls.map(l => l -> model.eval(ctx.mkIntConst(s"log_$l"), false).toString.toInt)
        Some((n.toMap, v.toMap))
      } else None
    }
  }

  def isSatForall(v: String): Boolean = {
    val formulas = {
      acceptFormulas.map(_.renameVars {
        case Left(i)  => s"int_$i"
        case Right(l) => s"log_$l"
      }) :+ parikhImageFormula.renameVars {
        case Left(i)  => s"bound_$i"
        case Right(l) => s"log_$l"
      }
    }
    withZ3Context { (ctx) =>
      val solver = ctx.mkSolver("LIA")
      val z3Exprs = formulas.map(Presburger.Formula.formulaToZ3Expr(ctx, Map.empty[String, z3.IntExpr], _))
      val freevars = formulas.map(_.freeVars.toSet).reduce(_ ++ _) - s"int_$v"
      val existsExpr = ctx.mkExists(freevars.toArray.map(ctx.mkIntConst), ctx.mkAnd(z3Exprs:_*), 0, null, null, null, null)
      val impliesExpr = ctx.mkImplies(ctx.mkGe(ctx.mkIntConst(s"int_$v"), ctx.mkInt(1)), existsExpr)
      val forallExpr = ctx.mkForall(Array(ctx.mkIntConst(s"int_$v")), impliesExpr, 0, null, null, null, null)
//      println(forallExpr)
      solver.add(forallExpr)
      val result = solver.check()
      result == z3.Status.SATISFIABLE
    }
  }

  /** Returns (w, w') such that this transduces w to (w', v). */
  def inputOutputFor(v: Map[L, Int]): (Seq[A], Seq[B]) = {
    val enft = toLogVectorEpsNFT
    val in = enft.takeInputFor(v, u => u.exists { case (l, i) => i > v(l) })
    val (out, _) = transduce(in).find { case (out, u) => v.forall { case (l, i) => i == u(l) } }.get
    (in, out)
  }

  /** Returns an input string, vector and an output string of this PSST if exists. */
  def outputOption: Option[(Seq[A], Map[I, Int], Seq[B])] = {
    ilVectorOption.map {
      case (n, v) =>
        val (in, out) = inputOutputFor(v)
        (in, n, out)
    }
  }
}

private case class MonoidParikhSST[Q, A, B, X, Y, L, I](
    states: Set[Q],
    inSet: Set[A],
    xs: Set[X],
    ys: Set[Y],
    ls: Set[L],
    is: Set[I],
    edges: Set[(Q, A, Update[X, Update[Y, B]], Map[L, Int], Q)],
    q0s: Set[Q],
    outGraph: Set[(Q, (Cupstar[X, Update[Y, B]], Cupstar[Y, B]), Map[L, Int])],
    acceptFormulas: Seq[Presburger.Formula[Either[I, L]]]
) {
  import MSST.{M1, gamma, proj}
  def toParikhSST: ParikhSST[Int, A, B, (X, Y, Boolean), L, I] = {
    type S = Map[X, M1[Y]]
    type NQ = (Q, S)
    type Z = (X, Y, Boolean)
    val zs: Set[Z] = for {
      x <- xs
      y <- ys
      b <- List(true, false)
    } yield (x, y, b)
    implicit val updateYMonoid: Monoid[Update[Y, Cop[Z, B]]] = ys

    def iota(s: S)(x: X): Update[Y, Cop[Z, B]] = {
      val prePost =
        for (y <- ys; b <- List(false, true))
          yield ((y, b) -> (if (b) List((x, y, b))
                            else List((x, y, b))))
      gamma(ys)(s(x), prePost.toMap).view
        .mapValues(_.map { case Cop1(y) => Cop1(y); case Cop2(z) => Cop2(Cop1(z)) })
        .toMap
    }

    def embedUpdate[X, A, B](m: Update[X, B]): Update[X, Cop[A, B]] = {
      m.view
        .mapValues(_.map {
          case Cop1(x) => Cop1(x)
          case Cop2(b) => Cop2(Cop2(b))
        })
        .toMap
    }

    def assignFold(s: S, alpha: Cupstar[X, Update[Y, B]]): Update[Y, Cop[Z, B]] = {
      val iotaS = iota(s) _
      val ms: List[Update[Y, Cop[Z, B]]] = alpha.map {
        case Cop1(x) => iotaS(x)
        case Cop2(m) => embedUpdate(m)
      }
      Monoid.fold(ms)
    }

    def nextState(s: S, mu: Update[X, Update[Y, B]]): (S, Update[Z, B]) = {
      val cache = xs.map(x => x -> proj(assignFold(s, mu(x)))).toMap
      val nextS = cache.map { case (x, (perm, _)) => x -> perm }
      val nextU: Update[Z, B] = zs.map { case (x, y, b) => (x, y, b) -> cache(x)._2(y, b) }.toMap
      (nextS, nextU)
    }

    val transOne: Map[(Q, A), Set[(Q, Update[X, Update[Y, B]], Map[L, Int])]] = graphToMap(edges) {
      case (q, a, m, v, r) => (q, a) -> (r, m, v)
    }

    def nextStates(q: NQ, a: A): Set[(NQ, Update[Z, B], Map[L, Int])] = q match {
      case (q, s) =>
        transOne(q, a)
          .map {
            case (nextQ, mu, v) => {
              val (nextS, update) = nextState(s, mu)
              ((nextQ, nextS), update, v)
            }
          }
    }

    val newQ0s = {
      val id = ys.map(y => y -> List(y)).toMap
      val const = xs.map(x => x -> id).toMap
      q0s.map((_, const))
    }
    val (newStates, newEdges) = searchStatesInt(newQ0s, inSet)(nextStates)(
      { case (r, _, _)              => r },
      { case (q, a, (r, m, v), map) => (map(q), a, m, v, map(r)) }
    )
    val outF: Map[Q, Set[(Cupstar[X, Update[Y, B]], Cupstar[Y, B], Map[L, Int])]] = graphToMap(outGraph) {
      case (q, (xms, xbs), v) => q -> (xms, xbs, v)
    }
    val newOutGraph: Set[(Int, Cupstar[Z, B], Map[L, Int])] = {
      for {
        nq @ (q, s) <- newStates.keySet
        (xms, xbs, v) <- outF(q)
      } yield {
        val m = assignFold(s, xms)
        val assigned = xbs.flatMap {
          case Cop1(y) => m(y)
          case Cop2(b) => List(Cop2(Cop2(b)))
        }
        (newStates(nq), erase1(assigned), v)
      }
    }.toSet

    val isNewQ0 = newQ0s.map(newStates)
    val additionalEdges = for {
      (q, a, m, v, r) <- newEdges.iterator if isNewQ0(q)
    } yield (-1, a, m, v, r)
    val additionalOut = for {
      (q, xbs, v) <- newOutGraph.iterator if isNewQ0(q)
    } yield (-1, xbs, v)

    ParikhSST[Int, A, B, (X, Y, Boolean), L, I](
      newStates.values.toSet + (-1),
      inSet,
      zs,
      ls,
      is,
      Set.from(additionalEdges ++ newEdges),
      -1,
      Set.from(additionalOut ++ newOutGraph),
      acceptFormulas
    )
  }

}

object ParikhSST {
  type ParikhUpdate[L] = Map[L, Int]

  object Implicits {
    implicit class ParikhUpdateOps[L](ml: ParikhUpdate[L]) {
      def apply(lv: ParikhUpdate[L]) = ml.map { case (l, i) => l -> (i + lv(l)) }
    }
  }
}
