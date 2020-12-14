package com.github.kmn4.sst

import smtlib.theories
import smtlib.theories.Ints
import smtlib.theories.experimental.Strings
import smtlib.trees.{Terms => SMTTerms}
import smtlib.trees.Terms.{Term => SMTTerm, SNumeral, SString, Sort}
import smtlib.trees.{Commands => SMTCommands}
import smtlib.trees.Commands.{Command => SMTCommand}
import ParikhSolver._
import Presburger.Sugar._
import Constraint.Transduction
import Solver.{SimpleQualID, SimpleApp, SimpleTransduction, expectRegExp}

class ParikhSolver(options: Solver.SolverOption) {

  def setLogic(logic: SMTCommands.Logic): Unit = ()

  // temp_*, len_*, user_*
  val freshTemp = {
    var varID = 0
    () => {
      varID += 1
      s"temp_$varID"
    }
  }

  var env = Map.empty[String, Sort]
  var constraints = Seq.empty[ParikhConstraint]

  def declareConst(name: SMTTerms.SSymbol, sort: SMTTerms.Sort): Unit = {
    val SMTTerms.SSymbol(s) = name
    sort match {
      case Ints.IntSort() | Strings.StringSort() => env += (s -> sort)
      case _                                     => throw new Exception(s"${sort.getPos}: Unsupported sort: ${sort}")
    }
  }

  def declareFun(name: SMTTerms.SSymbol, paramSorts: Seq[SMTTerms.Sort], returnSort: SMTTerms.Sort): Unit =
    paramSorts match {
      case Nil => declareConst(name, returnSort)
      case _   => throw new Exception(s"${name.getPos}: Only constants are supported")
    }

  def assert(assertion: SMTTerm): Unit = {
    val (c, cs) = expectConstraint(assertion)
    constraints ++= (cs :+ c)
  }

  class Checker(psst: SolverPSST[Char, String], idxVar: Map[Int, String]) {
    // _1: Int var -> value, _2: Log var -> value
    val witnessVector: () => Option[(Map[String, Int], Map[Int, Int])] = Cacher { psst.ilVectorOption }.getOrCalc _
    // _1: Str var -> value, _2: Int var -> value
    val models: () => Option[(Map[String, String], Map[String, Int])] = Cacher {
      witnessVector().map {
        case (iv, lv) =>
          val (_, output) = psst.inputOutputFor(lv)
          (parseStrModel(output), parseIntModel(iv))
      }
    }.getOrCalc _
    def parseStrModel(output: Seq[Option[Char]]): Map[String, String] = {
      var buf = output
      var idx = 0
      var res = Map.empty[String, Seq[Char]]
      while (buf.nonEmpty) {
        val took = buf.takeWhile(_.nonEmpty).flatten
        buf = buf.drop(took.length + 1)
        res += (idxVar(idx) -> took)
        idx += 1
      }
      res.view.mapValues(_.mkString).toMap
    }
    def parseIntModel(iv: Map[String, Int]): Map[String, Int] =
      iv.collect { case (name, value) if name.indexOf("user_") == 0 => name.drop(5) -> value }
    def checkSat(): Boolean = witnessVector().nonEmpty
    def getModel(): Option[(Map[String, String], Map[String, Int])] = models()
  }

  val (checker, resetChecker) = {
    val c = Cacher[Checker] {
      val (psst, idxVar) = Compiler.compile(constraints)
      new Checker(psst, idxVar)
    }
    (c.getOrCalc _, c.reset _)
  }

  def checkSat(): Unit =
    if (checker().checkSat()) println("sat")
    else println("unsat")

  def getModel(): Unit = {
    checker().getModel() match {
      case Some((sModel, iModel)) =>
        for ((name, value) <- sModel) println(s"(define-fun $name () String ${value})")
        for ((name, value) <- iModel) println(s"(define-fun $name () Int ${value})")
      case None => println("Cannot get model")
    }
  }

  // arbitrary int expression.
  // ex. (+ (str.indexof x "a" 0) 1) => Add(temp_i, 1), [x ∈ IndexOfFromZero("a", temp_i)]
  def expectInt(t: SMTTerm): (Presburger.Term[String], Seq[ParikhConstraint]) = {
    t match {
      case SNumeral(i)        => (Presburger.Const(i.toInt), Seq.empty)
      case SimpleQualID(name) => (Presburger.Var(s"user_$name"), Seq.empty)
      case Ints.Neg(t) =>
        val (pt, cs) = expectInt(t)
        (Presburger.Const(0) - pt, cs)
      case Strings.Length(SimpleQualID(name)) =>
        val lenVar = s"len_${name}"
        (Presburger.Var(lenVar), Seq(ParikhAssertion(name, ParikhLanguage.Length(lenVar))))
      case SimpleApp("+", ts) =>
        val (pts, css) = ts.map(expectInt).unzip
        (Presburger.Add(pts), css.flatten)
      case Ints.Sub(t1, t2) =>
        val (pt1, cs1) = expectInt(t1)
        val (pt2, cs2) = expectInt(t2)
        (Presburger.Sub(pt1, pt2), cs1 ++ cs2)
      case Strings.Experimental.IndexOf(SimpleQualID(name), SString(w), SNumeral(c)) if c == 0 =>
        val newVar = freshTemp()
        val constr = ParikhAssertion(name, ParikhLanguage.IndexOfFromZero(w, newVar))
        (Presburger.Var(newVar), Seq(constr))
      case Strings.Experimental.IndexOf(SimpleQualID(name), SString(w), e) =>
        val (pt, cs) = expectInt(e)
        // val constr = ParikhAssertion(StringVar(name), ParikhLanguage.IndexOf(w, newVar, i))
        // (Presburger.Var(newVar), cs :+ constr)
        ???
      case _ =>
        throw new Exception(s"${t.getPos}: Cannot interpret given S-expression ${t} as int expression")
    }
  }

  // (substr x t1 t2) where t1 or t2 is not numeral
  def expectParikhTransduction(
      t: SMTTerm
  ): (String, ParikhTransduction[Char, String], Seq[ParikhConstraint]) = t match {
    case Strings.Substring(SimpleQualID(rhsVar), t1, t2) =>
      val (pt1, cs1) = expectInt(t1)
      val (pt2, cs2) = expectInt(t2)
      val (from, len) = (freshTemp(), freshTemp())
      (
        rhsVar,
        ParikhTransduction.Substr(from, len),
        cs1 ++ cs2 ++ Seq(pt1 === Presburger.Var(from), pt2 === Presburger.Var(len))
      )
    case _ => throw new Exception(s"${t.getPos}: Cannot interpret given S-expression ${t} as transduction")
  }

  object IntConstraint {
    val binary = Seq[
      (
          smtlib.theories.Operations.Operation2,
          (Presburger.Term[String], Presburger.Term[String]) => Presburger.Formula[String]
      )
    ](
      (theories.Core.Equals, Presburger.Eq.apply[String] _),
      (Ints.LessThan, Presburger.Lt.apply[String] _),
      (Ints.LessEquals, Presburger.Le.apply[String] _),
      (Ints.GreaterThan, Presburger.Gt _),
      (Ints.GreaterEquals, Presburger.Ge _)
    )
    def unapply(t: SMTTerm): Option[(PureIntConstraint, Seq[ParikhConstraint])] = {
      val binOpt = binary.find { case (op, _) => op.unapply(t).nonEmpty }.map {
        case (op, constructor) =>
          val Some((t1, t2)) = op.unapply(t)
          val (pt1, cs1) = expectInt(t1)
          val (pt2, cs2) = expectInt(t2)
          (constructor(pt1, pt2), cs1 ++ cs2)
      }
      if (binOpt.nonEmpty) return Some(binOpt.get)
      t match {
        case theories.Core.Not(IntConstraint((f, cs))) => Some((Presburger.Not(f), cs))
        case theories.Core.And(ts @ _*) =>
          val sub = ts.map(unapply)
          if (sub.exists(_.isEmpty)) return None
          val (fs, css) = sub.map(_.get).unzip
          Some((Presburger.Conj(fs), css.flatten))
        case theories.Core.Or(ts @ _*) =>
          val sub = ts.map(unapply)
          if (sub.exists(_.isEmpty)) return None
          val (fs, css) = sub.map(_.get).unzip
          Some((Presburger.Disj(fs), css.flatten))
        case _ => None
      }
    }
  }

  object Concat {
    def unapply(t: SMTTerm): Option[Seq[Either[Seq[Char], String]]] = t match {
      case Strings.Concat(ts @ _*) if ts.forall {
            case SimpleQualID(_) | SString(_) => true
            case _                            => false
          } =>
        val wordAndVars = ts.collect {
          case SString(w)         => Left(w.toSeq)
          case SimpleQualID(name) => Right(name)
        }
        Some(wordAndVars)
      case _ => None
    }
  }

  // (assert t)
  // _1 is t, and _2 is int equations and / or Parick assertions (for length).
  def expectConstraint(t: SMTTerm): (ParikhConstraint, Seq[ParikhConstraint]) = t match {
    // If (= x t) and x is string variable then t is transduction
    case theories.Core.Equals(SimpleQualID(name), t) if env(name) == Strings.StringSort() =>
      t match {
        case SimpleTransduction(rhsStringVar, trans) =>
          (ParikhAssignment(name, trans, rhsStringVar), Seq.empty)
        case Concat(wordAndVars) => (CatAssignment(name, wordAndVars), Seq.empty)
        case _ =>
          val (rhs, trans, cs) = expectParikhTransduction(t)
          (ParikhAssignment(name, trans, rhs), cs)
      }
    // Other equalities are between ints.
    case IntConstraint(f, cs) => (f, cs)
    case Strings.InRegex(SimpleQualID(name), t) =>
      val re = expectRegExp(t)
      (ParikhAssertion(name, re), Seq.empty)
    case _ => throw new Exception(s"${t.getPos}: Unsupported assertion: ${t}")
  }

  def execute(cmd: SMTCommand): Unit = cmd match {
    case SMTCommands.SetLogic(logic)                          => setLogic(logic)
    case SMTCommands.DeclareConst(name, sort)                 => declareConst(name, sort)
    case SMTCommands.DeclareFun(name, paramSorts, returnSort) => declareFun(name, paramSorts, returnSort)
    case SMTCommands.Assert(assertion)                        => assert(assertion)
    case SMTCommands.CheckSat()                               => checkSat()
    case SMTCommands.GetModel()                               => getModel()
    case _                                                    => throw new Exception(s"${cmd.getPos}: Unsupported command: ${cmd}")
  }

  def executeScript(script: smtlib.trees.Commands.Script): Unit = script.commands.foreach(execute)
}

object ParikhSolver {

  type SolverPSST[C, I] = ParikhSST[Int, Option[C], Option[C], Int, Int, I]

  sealed trait ParikhLanguage[C, I] {
    def toParikhAutomaton(alphabet: Set[C]): ParikhAutomaton[Int, C, Int, I]
    def usedAlphabet: Set[C]
  }

  object ParikhLanguage {
    implicit class FromRegExp[C, I](val re: RegExp[C]) extends ParikhLanguage[C, I] {

      def usedAlphabet: Set[C] = re.usedAlphabet

      def toParikhAutomaton(alphabet: Set[C]): ParikhAutomaton[Int, C, Int, I] =
        re.toNFA(alphabet).toDFA.toParikhAutomaton.renamed
    }
    case class Length[A, I](lenVar: I) extends ParikhLanguage[A, I] {
      def usedAlphabet: Set[A] = Set.empty

      def toParikhAutomaton(alphabet: Set[A]): ParikhAutomaton[Int, A, Int, I] =
        ParikhAutomaton(
          Set(0),
          alphabet,
          Set(0),
          Set(lenVar),
          alphabet.map(a => (0, a, Map(0 -> 1), 0)),
          0,
          Set((0, Map(0 -> 0))),
          Seq(Presburger.Eq(Presburger.Var(Left(lenVar)), Presburger.Var(Right(0))))
        )
    }
    case class IndexOfFromZero[A, I](target: Seq[A], iName: I) extends ParikhLanguage[A, I] {
      def usedAlphabet: Set[A] = target.toSet

      def toParikhAutomaton(alphabet: Set[A]): ParikhAutomaton[Int, A, Int, I] = {
        import Presburger._
        type Q = Int
        type L = Int
        type Edges = Iterable[(Q, A, Map[L, Int], Q)]
        val x = 0
        type T = Term[Either[I, L]]
        val i: T = Var(Left(iName))
        val input: T = Var(Right(0))
        val output: T = Var(Right(1))
        val dfa = Solver.postfixDFA(target, alphabet)
        val states = dfa.states
        val edges: Edges = {
          for {
            q <- states
            a <- alphabet
          } yield {
            val r = dfa.transition.getOrElse((q, a), q)
            val skipped =
              if (dfa.finalStates(r)) 0
              else q + 1 - r
            val v = Map(0 -> 1, 1 -> skipped)
            (q, a, v, r)
          }
        }
        val outGraph =
          // On each state q, DFA has partially matched prefix of target string.
          states.map(q => (q, Map(0 -> 0, 1 -> (q % target.length))))
        val acceptFormulas = Seq(
          output >= input ==> (i === -1),
          output < input ==> (i === output)
        )
        ParikhAutomaton[Q, A, L, I](
          states,
          alphabet,
          Set(0, 1),
          Set(iName),
          edges.toSet,
          dfa.q0,
          outGraph,
          acceptFormulas
        )
      }
    }
  }

  trait ParikhTransduction[C, I] {
    def usedAlphabet: Set[C]
    def toParikhSST(alphabet: Set[C]): ParikhSST[Int, C, C, Int, Int, I]
    def toSolverPSST(alphabet: Set[C], lhsStringVarIdx: Int, rhsStringVarIdx: Int): SolverPSST[C, I] = {
      sealed trait X
      case object XIn extends X
      case class XJ(x: Int) extends X
      type Q = (Int, Int)
      type A = Option[C]
      type UpdateX = Update[X, A]
      type UpdateL = Map[Int, Int]
      type Edges = Iterable[(Q, A, UpdateX, UpdateL, Q)]
      val j = rhsStringVarIdx
      val jsst = this.toParikhSST(alphabet)
      val xjs: Set[X] = jsst.xs.map(XJ.apply)
      val xj = xjs.head
      val base =
        Solver
          .solverNsstTemplate[C, X](
            lhsStringVarIdx,
            alphabet,
            XIn,
            xjs,
            List(Cop1(XIn), Cop1(xj), Cop2(None))
          )
          .toParikhSST[Int, I](jsst.ls)
      val xs = base.xs
      val updates: Monoid[UpdateX] = updateMonoid(xs)
      val states: Set[Q] = base.states - ((j, 0)) ++ jsst.states.map((j, _))
      val edges: Edges = {
        val baseNoJ = base.edges.filter {
          case (q, a, m, v, r) => (q._1 != j) && (r._1 != j)
        }
        def unit(a: A): UpdateX = updates.unit + (XIn -> List(Cop1(XIn), Cop2(a)))
        def reset(a: A): UpdateX = xs.map(_ -> Nil).toMap + (XIn -> List(Cop1(XIn), Cop2(a)))
        val toJ =
          ((j - 1, 0), None, unit(None), jsst.ls.map(_ -> 0).toMap, (j, jsst.q0))
        def embedList(l: Cupstar[Int, C]): Cupstar[X, A] = l.map(_.map1(XJ.apply)).map(_.map2(Option.apply))
        def embedUpdate(m: Update[Int, C]): Update[X, A] = m.map { case (x, l) => XJ(x) -> embedList(l) }
        val withinJ: Edges = jsst.edges.map {
          case (q, a, m, v, r) =>
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
  }

  object ParikhTransduction {
    implicit class NSSTTransductionIsParikhTransduction[C, I](trans: Transduction[C])
        extends ParikhTransduction[C, I] {
      def usedAlphabet: Set[C] = trans.usedAlphabet

      def toParikhSST(alphabet: Set[C]): ParikhSST[Int, C, C, Int, Int, I] =
        trans.toSST(alphabet).toParikhSST

    }

    case class Substr[A, I](idxName: I, lenName: I) extends ParikhTransduction[A, I] {

      def usedAlphabet: Set[A] = Set.empty

      def toParikhSST(alphabet: Set[A]): ParikhSST[Int, A, A, Int, Int, I] = {
        import Presburger._
        val X = 0
        type T = Term[Either[I, Int]]
        val idx: T = Var(Left(idxName))
        val len: T = Var(Left(lenName))
        val input: T = Var(Right(0))
        val taken: T = Var(Right(1))
        val sought: T = Var(Right(2))
        val unit: (Update[Int, A], ParikhSST.ParikhUpdate[Int]) =
          (Map(X -> List(Cop1(X))), Map(0 -> 1, 1 -> 0, 2 -> 0))
        val edges = alphabet
          .flatMap { a =>
            val (unitX, unitH) = unit
            val seek = (unitX, unitH + (2 -> 1))
            val take = (Map(X -> List(Cop1(X), Cop2(a))), unitH + (1 -> 1))
            val ignore = unit
            Iterable(
              (0, a, seek, 0),
              (0, a, take, 1),
              (1, a, take, 1),
              (1, a, ignore, 2),
              (2, a, ignore, 2)
            )
          }
          .map { case (q, a, (mx, mh), r) => (q, a, mx, mh, r) }
        val acceptFormulas = {
          val idxOutOrNegLen = idx < 0 || idx >= input || len <= 0
          Seq(
            idxOutOrNegLen ==> (taken === 0),
            (!idxOutOrNegLen && len <= input - idx) ==> (sought === idx && taken === len),
            (!idxOutOrNegLen && len > input - idx) ==> (sought === idx && taken === input - idx)
          )
        }
        ParikhSST[Int, A, A, Int, Int, I](
          Set(0, 1, 2),
          alphabet,
          Set(X),
          Set(0, 1, 2),
          Set(idxName, lenName),
          edges,
          0,
          (0 to 2).map((_, List(Cop1(X)), (0 to 2).map(h => h -> 0).toMap)).toSet,
          acceptFormulas
        )
      }
    }
  }

  sealed trait ParikhConstraint {
    def usedAlphabet: Set[Char]
    def dependerVars: Seq[String]
    def dependeeVars: Seq[String]
  }
  sealed trait AtomicAssignment extends ParikhConstraint {
    def toSolverPSST(varIdx: Map[String, Int])(alphabet: Set[Char]): SolverPSST[Char, String]
  }
  case class ParikhAssignment(
      lhsStringVar: String,
      trans: ParikhTransduction[Char, String],
      rhsStringVar: String
  ) extends AtomicAssignment {

    override def dependerVars: Seq[String] = Seq(lhsStringVar)

    override def dependeeVars: Seq[String] = Seq(rhsStringVar)

    def usedAlphabet: Set[Char] = trans.usedAlphabet
    def toSolverPSST(varIdx: Map[String, Int])(alphabet: Set[Char]): SolverPSST[Char, String] =
      trans.toSolverPSST(alphabet, varIdx(lhsStringVar), varIdx(rhsStringVar))
  }
  // Left(word), Right(stringVar)
  case class CatAssignment(lhsStringVar: String, wordAndVars: Seq[Either[Seq[Char], String]])
      extends AtomicAssignment {

    override def dependerVars: Seq[String] = Seq(lhsStringVar)

    override def dependeeVars: Seq[String] = wordAndVars.flatMap(_.toOption)

    def usedAlphabet: Set[Char] = wordAndVars.flatMap(_.left.toOption.map(_.toSet).getOrElse(Set.empty)).toSet
    def toSolverPSST(varIdx: Map[String, Int])(alphabet: Set[Char]): SolverPSST[Char, String] =
      Solver.concatNSST(varIdx(lhsStringVar), wordAndVars.map(_.map(varIdx)), alphabet).toParikhSST
  }
  case class ParikhAssertion(stringVar: String, lang: ParikhLanguage[Char, String]) extends ParikhConstraint {

    override def dependerVars: Seq[String] = Seq.empty

    override def dependeeVars: Seq[String] = Seq(stringVar)

    def usedAlphabet: Set[Char] = lang.usedAlphabet
  }
  type PureIntConstraint = Presburger.Formula[String]
  implicit class IntConstraintIsParikhConstraint(val formula: PureIntConstraint) extends ParikhConstraint {

    override def dependerVars: Seq[String] = Seq.empty

    override def dependeeVars: Seq[String] = Seq.empty

    def usedAlphabet: Set[Char] = Set.empty

  }
  object IntConstraintIsParikhConstraint {
    def unapply(c: ParikhConstraint): Option[PureIntConstraint] = c match {
      case (c: IntConstraintIsParikhConstraint) => Some(c.formula)
      case _                                    => None
    }
  }

  object Compiler {
    def stringVarIndex(constraints: Seq[ParikhConstraint]): Map[String, Int] = {
      val dependers = constraints.flatMap(_.dependerVars).distinct
      val dependees = constraints.flatMap(_.dependeeVars).distinct
      val independents = dependees.diff(dependers)
      (independents ++ dependers).distinct.zipWithIndex.toMap
    }

    /** Returns ParikhAutomaton that accepts an input iff it meets constriant given by `pas`.
      * That is, it reads an input of the form w0#w1#...w(n-1)# (where n = dfas.length and # = None) and
      * accepts if pas(i) accepts w(i) for all i. */
    def solverPA[Q, A, L, I](
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

    def compileParikhAssertions(
        assertions: Map[Int, Seq[ParikhLanguage[Char, String]]],
        alphabet: Set[Char],
        lastVarIdx: Int
    ): SolverPSST[Char, String] = {
      require(
        lastVarIdx >= assertions.map(_._1).max,
        "All LHS of PA assertions should be less than or equal to max LHS of assignments."
      )
      val idxRegularsParikhs = {
        assertions.map {
          case (i, langs) =>
            val rs = langs.collect { case (l: ParikhLanguage.FromRegExp[Char, String]) => l.re }
            val ps = langs.filterNot(_.isInstanceOf[ParikhLanguage.FromRegExp[Char, String]])
            i -> (rs, ps)
        }
      }
      val idxPA = idxRegularsParikhs.view.mapValues {
        case (rs, ps) =>
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
      val inSet = addNone(alphabet)
      val universalPA = ParikhAutomaton.universal[Int, Char, Int, String](0, alphabet)
      solverPA((0 to lastVarIdx).map(idxPA.getOrElse(_, universalPA)), 0).toParikhSST.renamed
    }

    def compileTriple(
        assignments: Seq[
          (Int, Set[Char] => SolverPSST[Char, String])
        ], // ([lhsVarIdx], [corresponding solver PSST])
        assertions: Map[Int, Seq[ParikhLanguage[Char, String]]], // [string var idx] in [Parikh langs]
        arithFormulas: Seq[PureIntConstraint] // formula over int variables
    )(alphabet: Set[Char]): SolverPSST[Char, String] = {
      require(
        assignments.map(_._1).sliding(2).forall(l => l.length < 2 || l(1) == l(0) + 1),
        "Not straight-line"
      )
      val lastPSST = {
        val lastVarIdx = assignments.last._1
        val p = compileParikhAssertions(assertions, alphabet, lastVarIdx)
        val is = arithFormulas.flatMap(_.freeVars)
        val formulas = arithFormulas.map(_.renameVars[Either[String, Int]](Left.apply))
        p.copy(is = p.is ++ is, acceptFormulas = p.acceptFormulas ++ formulas)
      }
      val assignmentPSSTs = assignments.map(_._2(alphabet))
      (assignmentPSSTs :+ lastPSST).reduce(_ compose _)
    }

    // _2: Index of string in PSST output -> String var name
    def compile(constraints: Seq[ParikhConstraint]): (SolverPSST[Char, String], Map[Int, String]) = {
      val varIdx = stringVarIndex(constraints)
      val assignments = constraints.collect {
        case a @ ParikhAssignment(lhs, trans, rhs) => (varIdx(lhs), a.toSolverPSST(varIdx) _)
        case a @ CatAssignment(lhs, wordAndVars)   => (varIdx(lhs), a.toSolverPSST(varIdx) _)
      }
      val assertions = constraints.collect { case ParikhAssertion(sVar, lang)          => (varIdx(sVar), lang) }
      val arithFormula = constraints.collect { case IntConstraintIsParikhConstraint(f) => f }
      val alphabet = constraints.flatMap(_.usedAlphabet).toSet
      val psst = compileTriple(assignments, assertions.groupMap(_._1)(_._2), arithFormula)(alphabet)
      (psst, varIdx.map { case (x, i) => i -> x })
    }

  }
}
