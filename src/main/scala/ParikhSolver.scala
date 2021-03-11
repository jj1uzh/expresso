// TODO rename this file

package com.github.kmn4.sst

import com.github.kmn4.sst.ParikhSolver._
import com.github.kmn4.sst.language.Constraint._
import com.github.kmn4.sst.language._
import com.github.kmn4.sst.machine._
import com.github.kmn4.sst.math.Presburger.Sugar._
import com.github.kmn4.sst.math._
import com.microsoft.z3
import com.typesafe.scalalogging.Logger
import smtlib.theories.Ints
import smtlib.theories.experimental.Strings
import smtlib.theories.experimental.Strings.StringSort
import smtlib.theories.{Core => CoreTheory}
import smtlib.trees.Commands.{Command => SMTCommand}
import smtlib.trees.Terms
import smtlib.trees.Terms.FunctionApplication
import smtlib.trees.Terms.QualifiedIdentifier
import smtlib.trees.Terms.SNumeral
import smtlib.trees.Terms.SString
import smtlib.trees.Terms.SSymbol
import smtlib.trees.Terms.SimpleIdentifier
import smtlib.trees.Terms.Sort
import smtlib.trees.Terms.{Term => SMTTerm}
import smtlib.trees.{Commands => SMTCommands}
import smtlib.trees.{Terms => SMTTerms}

class ParikhSolver(
    print: Boolean = false,
    logger: Logger = Logger("nop"),
    alphabet: Set[Char] = Set.empty // ADDED to the alphabet of constraints
) {
  type ParikhConstraint = Constraint.ParikhConstraint[String]

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
    val (c, cs) = expectConstraint(expandMacro(assertion))
    constraints ++= (cs :+ c)
  }

  def printLine(x: Any): Unit = if (print) println(x)

  val checker = new strategy.PreImageStrategy(logger)

  def checkSat(): Unit = {
    // FIXME Set.empty
    val input = transform(constraints, alphabet, Set.empty)
    val sat = checker.checkSat(input)
    logger.trace(s"checking done, ${if (sat) "SAT" else "UNSAT"}")
    if (sat) printLine("sat")
    else printLine("unsat")
  }

  def getModel(): Unit = {
    checker.getModel() match {
      case Some((sModel, iModel)) =>
        val stringVars = sortStringVars(constraints)
        logger.trace(s"got model ${(sModel, iModel)}")
        for ((value, idx) <- sModel.zipWithIndex)
          printLine(s"""(define-fun ${stringVars(idx)} () String "${value}")""")
        for ((name, value) <- iModel) printLine(s"(define-fun $name () Int ${value})")
      case None => printLine("Cannot get model")
    }
  }

  def expandMacro(t: SMTTerm): SMTTerm = t match {
    case CoreTheory.Not(CoreTheory.Equals(SimpleQualID(s1), SimpleQualID(s2)))
        if env.get(s1).exists(_ == StringSort()) && env.get(s2).exists(_ == StringSort()) =>
      val (x, y) = (SimpleQualID(s1), SimpleQualID(s2))
      val i = SimpleQualID(freshTemp())
      // codeAt(x, i) != codeAt(y, i)
      CoreTheory.Not(CoreTheory.Equals(CodeAt(x, i), CodeAt(y, i)))
    case _ => t
  }

  // TODO ここではない
  object CodeAt {
    def apply(x: SMTTerm, i: SMTTerm): SMTTerm = SimpleApp("code_at", x, i)
    def unapply(t: SMTTerm): Option[(SMTTerm, SMTTerm)] = t match {
      case SimpleApp("code_at", Seq(x, i)) => Some(x, i)
      case _                               => None
    }
  }

  def expectRegExp(t: SMTTerm): RegExp[Char] =
    t match {
      case Strings.ToRegex(SString(s)) =>
        if (s.nonEmpty)
          s.map[RegExp[Char]](CharExp.apply).reduce[RegExp[Char]] { case (e1, e2) => CatExp(e1, e2) }
        else EpsExp
      case Strings.Regex.*(t) => StarExp(expectRegExp(t))
      case Strings.Regex.+(t) =>
        val re = expectRegExp(t)
        CatExp(re, StarExp(re))
      case Strings.Regex.Concat(ts @ _*) =>
        ts.tail.foldLeft(expectRegExp(ts.head)) { case (acc, t) => CatExp(acc, expectRegExp(t)) }
      case Strings.Regex.Union(ts @ _*) =>
        ts.tail.foldLeft(expectRegExp(ts.head)) { case (acc, t) => OrExp(acc, expectRegExp(t)) }
      case Strings.Regex.Range(SString(c1), SString(c2)) if c1.length == 1 && c2.length == 1 =>
        throw new NotImplementedError("re.range is not implemented")
      case SimpleApp("re.comp", Seq(e)) => CompExp(expectRegExp(e))
      case Strings.Regex.AllChar()      => DotExp
      case SimpleQualID("re.all")       => StarExp(DotExp)
      case _                            => throw new Exception(s"Cannot interpret given S-expression as regular expression: $t")
    }

  type SolverOption = Unit

  def expectPCRE(t: SMTTerm): Replacer.PCRE[Char, Int] = {
    type RE = Replacer.PCRE[Char, Int]
    var group = 0
    def nextGroup(): Int = {
      group += 1
      group
    }
    def aux(t: SMTTerm): RE = t match {
      case SimpleApp("str.to_pcre", Seq(SString(w))) =>
        w.map[RE](c => Replacer.PCRE.Chars(Set(c)))
          .fold[Replacer.PCRE[Char, Int]](Replacer.PCRE.Eps())(Replacer.PCRE.Cat.apply)
      case SimpleApp("pcre.alt", ts)   => ts.map(aux).reduce[RE](Replacer.PCRE.Alt.apply)
      case SimpleApp("pcre.++", ts)    => ts.map(aux).reduce[RE](Replacer.PCRE.Cat.apply)
      case SimpleApp("pcre.*", Seq(t)) => Replacer.PCRE.Greedy(aux(t))
      case SimpleApp("pcre.+", Seq(t)) =>
        val pcre = aux(t)
        Replacer.PCRE.Cat(pcre, Replacer.PCRE.Greedy(pcre))
      case SimpleApp("pcre.*?", Seq(t)) => Replacer.PCRE.NonGreedy(aux(t))
      case SimpleApp("pcre.group", Seq(t)) =>
        val group = nextGroup()
        Replacer.PCRE.Group(aux(t), group)
      case SimpleQualID("pcre.allchar") => Replacer.PCRE.AllChar()
      case _                            => throw new Exception(s"${t.getPos}: PCRE expected but found: $t")
    }
    aux(t)
  }

  def expectReplacement(t: SMTTerm): Replacer.Replacement[Char, Int] = t match {
    case SimpleApp("pcre.replacement", ts) =>
      Replacer.Replacement(
        ts.flatMap {
          case SString(w)            => w.map(Left.apply)
          case SNumeral(i) if i == 0 => Seq(Right(None))
          case SNumeral(i) if i > 0  => Seq(Right(Some(i.toInt)))
          case t                     => throw new Exception(s"${t.getPos}: PCRE Replacement component expected but found: $t")
        }
      )
    case _ => throw new Exception(s"${t.getPos}: PCRE Replacement expected but found: $t")
  }

  // arbitrary int expression.
  // ex. (+ (str.indexof x "a" 0) 1) => Add(temp_i, 1), [x ∈ IndexOfFromZero("a", temp_i)]
  def expectInt(t: SMTTerm): (Presburger.Term[String], Seq[ParikhConstraint]) = {
    def parseAndAbstract(t: SMTTerm): (String, Seq[ParikhConstraint]) = {
      val (pt, cs) = expectInt(t)
      pt match {
        case Presburger.Var(s) => (s, cs)
        case _ => {
          val newVar = freshTemp()
          (newVar, cs :+ (Presburger.Var(newVar) === pt))
        }
      }
    }
    t match {
      case SNumeral(i)        => (Presburger.Const(i.toInt), Seq.empty)
      case SimpleQualID(name) => (Presburger.Var(s"user_$name"), Seq.empty)
      case Ints.Neg(t) =>
        val (pt, cs) = expectInt(t)
        (Presburger.Const(0) - pt, cs)
      case Strings.Length(SimpleQualID(name)) =>
        val lenVar = s"len_${name}"
        (Presburger.Var(lenVar), Seq(ParikhAssertion(name, ParikhLanguage.Length(lenVar))))
      case CodeAt(SimpleQualID(name), i) if env.get(name).exists(_ == StringSort()) =>
        val c = freshTemp()
        val (j, cs) = parseAndAbstract(i)
        val assertion = ParikhAssertion(name, ParikhLanguage.CodeAt(j, c))
        (Presburger.Var(c), cs :+ assertion)
      case SimpleApp("+", ts) =>
        val (pts, css) = ts.map(expectInt).unzip
        (Presburger.Add(pts), css.flatten)
      case Ints.Sub(t1, t2) =>
        val (pt1, cs1) = expectInt(t1)
        val (pt2, cs2) = expectInt(t2)
        (Presburger.Sub(pt1, pt2), cs1 ++ cs2)
      case Strings.Experimental.IndexOf(SimpleQualID(name), SString(w), SNumeral(c)) if c == 0 =>
        val newVar = freshTemp()
        val constr = ParikhAssertion(name, ParikhLanguage.IndexOfConst(w, c.toInt, newVar))
        (Presburger.Var(newVar), Seq(constr))
      case Ints.Mul(SNumeral(c), t) =>
        val (pt, cs) = expectInt(t)
        (Presburger.Mult(Presburger.Const(c.toInt), pt), cs)
      case Strings.Experimental.IndexOf(SimpleQualID(name), SString(w), t) =>
        val (i, cs) = parseAndAbstract(t)
        val j = freshTemp()
        (Presburger.Var(j), cs :+ ParikhAssertion(name, ParikhLanguage.IndexOf(w, i, j)))
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

  object SimpleQualID {
    def apply(name: String): QualifiedIdentifier =
      QualifiedIdentifier(SimpleIdentifier(SSymbol(name)), None)
    def unapply(term: SMTTerm): Option[String] = term match {
      case QualifiedIdentifier(SimpleIdentifier(SSymbol(name)), None) => Some(name)
      case _                                                          => None
    }
  }

  object SimpleApp {
    def apply(name: String, args: SMTTerm*): SMTTerm = FunctionApplication(SimpleQualID(name), args)
    def unapply(term: SMTTerm): Option[(String, Seq[SMTTerm])] = term match {
      case FunctionApplication(SimpleQualID(name), terms) => Some((name, terms))
      case _                                              => None
    }
  }

  object SimpleTransduction {
    // (rhs, transduction)
    def unapply(e: SMTTerm): Option[(String, Transduction[Char])] =
      e match {
        case SimpleApp("str.replaceall", Seq(SimpleQualID(name), SString(target), SString(word))) =>
          Some((name, ReplaceAll(target, word)))
        case SimpleApp("str.replace_all", Seq(SimpleQualID(name), SString(target), SString(word))) =>
          Some((name, ReplaceAll(target, word)))
        case SimpleApp("str.replace_some", Seq(SimpleQualID(name), SString(target), SString(word))) =>
          Some((name, ReplaceSome(target, word)))
        case Strings.At(SimpleQualID(name), SimpleQualID(pos)) =>
          Some((name, At(pos.toInt)))
        case SimpleApp("str.replace_pcre", Seq(SimpleQualID(name), pcre, replacement)) =>
          Some((name, Replacer.ReplacePCRE(expectPCRE(pcre), expectReplacement(replacement))))
        case SimpleApp("str.replace_pcre_all", Seq(SimpleQualID(name), pcre, replacement)) =>
          Some((name, Replacer.ReplacePCREAll(expectPCRE(pcre), expectReplacement(replacement))))
        case SimpleApp("str.insert", Seq(SimpleQualID(name), SNumeral(pos), SString(word))) =>
          Some((name, Insert(pos.toInt, word)))
        case SimpleApp("str.reverse", Seq(SimpleQualID(name))) =>
          Some((name, Reverse()))
        case Strings.Substring(SimpleQualID(name), SNumeral(from), SNumeral(len)) =>
          Some((name, Substr(from.toInt, len.toInt)))
        case _ => None
      }
  }

  object IntConstraint {
    val binary = Seq[
      (
          smtlib.theories.Operations.Operation2,
          (Presburger.Term[String], Presburger.Term[String]) => Presburger.Formula[String]
      )
    ](
      (CoreTheory.Equals, Presburger.Eq.apply[String] _),
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
        case CoreTheory.Not(IntConstraint((f, cs))) => Some((Presburger.Not(f), cs))
        case CoreTheory.And(ts @ _*) =>
          val sub = ts.map(unapply)
          if (sub.exists(_.isEmpty)) return None
          val (fs, css) = sub.map(_.get).unzip
          Some((Presburger.Conj(fs), css.flatten))
        case CoreTheory.Or(ts @ _*) =>
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
    case CoreTheory.Equals(SimpleQualID(name), t) if env(name) == Strings.StringSort() =>
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

  def sortStringVars(constraints: Seq[ParikhConstraint]): Seq[String] = {
    val dependers = constraints.flatMap(_.dependerVars).distinct
    val dependees = constraints.flatMap(_.dependeeVars).distinct
    val independents = dependees.diff(dependers)
    (independents ++ dependers).distinct
  }

  // input: Seq[ParikhConstraint[String]], Set[Char], Set[String]
  // output: strategy.Input
  def transform(
      constraints: Seq[ParikhConstraint],
      additionalAlphabet: Set[Char],
      userIntVars: Set[String]
  ): strategy.Input = {
    val stringVars = sortStringVars(constraints)
    val varIdx = stringVars.zipWithIndex.toMap
    val alphabet = {
      val used = constraints.flatMap(_.usedAlphabet).toSet
      used ++ additionalAlphabet
    }
    val assignments = constraints.collect { case a: AtomicAssignment[String]         => a.renameVars(varIdx) }
    val assertions = constraints.collect { case a: ParikhAssertion[String]           => a.renameVars(varIdx) }
    val arithFormula = constraints.collect { case IntConstraintIsParikhConstraint(f) => f }
    strategy.Input(
      alphabet,
      stringVars.length,
      userIntVars,
      assignments,
      assertions,
      arithFormula
    )
  }

}

object ParikhSolver {
  case class SolverOption(print: Boolean = true, logger: Logger = Logger("nop"))

}

// object Compiler {

//   // TODO この名前だと Hoare 論理のアレみたいで misleading なので変える
//   case class Triple(
//       assignments: Seq[(Int, Set[Char] => PST, Seq[Int])], // (左辺, PST, 右辺)
//       assertions: Map[Int, Seq[ParikhLanguage[Char, String]]], // [string var idx] in [Parikh langs]
//       arithFormulas: Seq[PureIntConstraint] // formula over int variables
//   ) {
//     // 文字列変数の添字の最大値
//     def maxStringVarOption: Option[Int] = {
//       val iter1 = assignments.iterator.map(_._1)
//       val iter2 = assertions.iterator.map(_._1)
//       (iter1 ++ iter2).maxOption
//     }

//     // 文字列変数の添字の最大値
//     // なお，assignments が非空ならその最後のインデックスに一致するはず
//     def maxStringVar: Int = maxStringVarOption.get
//   }

//   // 入力: (文字列変数インデックス，PST), (文字列変数インデックス, PAs), 整数制約
//   // 出力: 逆像を計算していったときの conjunction の可能性を列挙するイテレータ
//   def compileTriple(
//       triple: Triple,
//       logger: Logger
//   )(alphabet: Set[Char]): Iterator[ParikhRelation[Int, Char, Int, String]] = {
//     val Triple(assignments, assertions, arithFormulas) = triple
//     require(
//       assignments.map(_._1).sliding(2).forall(l => l.length < 2 || l(1) == l(0) + 1),
//       "Not straight-line"
//     )
//     val initialConfig = organize(triple, alphabet)
//     iteratePreImage(initialConfig)
//     // val lastPSST = {
//     //   val lastVarIdx = assignments.lastOption.map(_._1).getOrElse(assertions.map(_._1).max)
//     //   val p = compileParikhAssertions(assertions, alphabet, lastVarIdx)
//     //   val is = arithFormulas.flatMap(_.freeVars)
//     //   val formulas = arithFormulas.map(_.renameVars[Either[String, Int]](Left.apply))
//     //   p.copy(is = p.is ++ is, acceptFormulas = p.acceptFormulas ++ formulas)
//     // }
//     // val assignmentPSSTs = assignments.map(_._2(alphabet))
//     // logger.trace("got the following PSSTs:")
//     // (assignmentPSSTs :+ lastPSST).zipWithIndex.foreach {
//     //   case (psst, i) => logger.trace(s"#$i: ${psst.sizes}")
//     // }
//     // (assignmentPSSTs :+ lastPSST).reduceLeft[SolverPSST[Char, String]] {
//     //   case (p1, p2) =>
//     //     logger.trace(s"compose ${p1.sizes} and ${p2.sizes}")
//     //     p1 compose p2
//     // }
//   }

//   def stringVarIndex(constraints: Seq[ParikhConstraint[String]]): Seq[(String, Int)] = {
//     val dependers = constraints.flatMap(_.dependerVars).distinct
//     val dependees = constraints.flatMap(_.dependeeVars).distinct
//     val independents = dependees.diff(dependers)
//     (independents ++ dependers).distinct.zipWithIndex
//   }

//   // _1 : LazyList
//   // _2 : index in relation → string variable name
//   // def compile(
//   //     constraints: Seq[ParikhConstraint],
//   //     additionalAlphabet: Set[Char],
//   //     logger: Logger
//   // ): (Iterator[SolverPR[Char, String]], Seq[String]) = {
//   //   logger.trace("start compilation")
//   //   val (varIdx, idxVar) = {
//   //     val vi = stringVarIndex(constraints)
//   //     (vi.toMap, vi.map(_._1))
//   //   }
//   //   val assignments = constraints.collect {
//   //     case a: AtomicAssignment =>
//   //       val rhs = a.dependeeVars.map(varIdx)
//   //       (varIdx(a.lhsStringVar), a.toExpPST _, rhs)
//   //   }
//   //   val assertions = constraints.collect { case ParikhAssertion(sVar, lang)          => (varIdx(sVar), lang) }
//   //   val arithFormula = constraints.collect { case IntConstraintIsParikhConstraint(f) => f }
//   //   val alphabet = {
//   //     val used = constraints.flatMap(_.usedAlphabet).toSet
//   //     used ++ additionalAlphabet
//   //   }
//   //   val triple = Triple(assignments, assertions.groupMap(_._1)(_._2), arithFormula)
//   //   val relGen = compileTriple(triple, logger)(alphabet)
//   //   logger.trace(s"compilation done")
//   //   (relGen, idxVar)
//   // }

// }
