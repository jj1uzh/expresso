package com.github.kmn4.expresso

import com.github.kmn4.expresso.language.Constraint._
import com.github.kmn4.expresso.language.RegExp._
import com.github.kmn4.expresso.language._
import com.github.kmn4.expresso.math.Presburger.Sugar._
import com.github.kmn4.expresso.math._
import com.github.kmn4.expresso.smttool.Strings
import com.github.kmn4.expresso.smttool.Strings.{StringSort}
import com.github.kmn4.expresso.smttool.{SimpleQualID, SimpleApp}
import com.typesafe.scalalogging.Logger
import smtlib.theories.Ints
import smtlib.theories.{Core => CoreTheory}
import smtlib.trees.Commands.{Command => SMTCommand}
import smtlib.trees.Terms
import smtlib.trees.Terms.SNumeral
// import smtlib.trees.Terms.SString
import smtlib.trees.Terms.Sort
import smtlib.trees.Terms.{Term => SMTTerm}
import smtlib.trees.{Commands => SMTCommands}
import smtlib.trees.{Terms => SMTTerms}
import smtlib.trees.TreeTransformer
import smtlib.trees.Tree

// 変数にプレフィックスを加えるのは Preprocessor (一時変数の導入をするから).
// Solver は変数にプレフィックスがついた制約を解き，そのモデルからプレフィックスをはずして出力する．
// VarProvider は両方から依存されるので独立したクラスとして定義する.
class VarProvider(tempPrefix: String, userPrefix: String) {
  private var c = 0
  def freshTemp(): String = {
    c += 1
    s"${tempPrefix}$c"
  }
  val TempVar = s"${tempPrefix}(.*)".r
  object UserVar {
    def apply(x: String): String = s"${userPrefix}$x"
    val pat = s"${userPrefix}(.*)".r
    def unapplySeq(w: String) = pat.unapplySeq(w)
  }
}

class Solver(
    val checker: strategy.Strategy,
    print: Boolean = false,
    alphabet: Set[Char] = Set.empty // ADDED to the alphabet of constraints
)(implicit
  logger: Logger = Logger("nop"),
) {
  type ParikhConstraint = Constraint.ParikhConstraint[String]

  def setLogic(logic: SMTCommands.Logic): Unit = ()

  // temp_*, user_*
  private val provider = new VarProvider("t", "u")
  val freshTemp = () => provider.freshTemp()

  var env = Map.empty[String, Sort]
  var constraints = Seq.empty[ParikhConstraint]
  var userIntVars = Set.empty[String]
  // プリプロセスによりユーザ変数が一時変数に置き換えられる場合がある (Preprocessor のコメント参照)
  private var userRepr: Map[String, String] = Map.empty

  def declareConst(name: SMTTerms.SSymbol, sort: SMTTerms.Sort): Unit = {
    val SMTTerms.SSymbol(s) = name
    sort match {
      case Ints.IntSort() =>
        userIntVars += s
        env += (s -> sort)
      case Strings.StringSort() => env += (s -> sort)
      case _                    => throw new Exception(s"${sort.getPos}: Unsupported sort: ${sort}")
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

  private def printLine(x: Any): Unit = if (print) println(x)

  private def withLogging[T](op: String)(body: => T): T = {
    logger.info(s"start $op")
    val res = body
    logger.info(s"$op done")
    res
  }

  private[expresso] def checkSat(): Unit = transform(constraints, alphabet) match {
    case Some(input) =>
      val sat = withLogging("checkSat()")(checker.checkSat(input))
      if (sat) printLine("sat")
      else printLine("unsat")
    case None =>
      printLine("unknown  ; input is not straight-line")
  }

  private[expresso] def getModel(): Option[(Map[String, String], Map[String, Int])] = {
    withLogging("getModel()")(checker.getModel() zip sortStringVars(constraints)) match {
      case Some(((ss, im), stringVars)) =>
        val sm = ss.zipWithIndex.map { case (value, idx) => stringVars(idx) -> value }.toMap
        val sModel =
          for ((provider.UserVar(x), representative) <- userRepr)
            yield x -> sm.getOrElse(representative, "")
        val iModel = for ((provider.UserVar(name), value) <- im) yield name -> value
        for ((name, value) <- sModel) printLine(s"""(define-fun ${name} () String "${value}")""")
        for ((name, value) <- iModel) printLine(s"(define-fun $name () Int ${value})")
        Some((sModel, iModel))
      case None =>
        printLine("Cannot get model")
        None
    }
  }

  // TODO expectConstraint を TreeTransformer で書き直す
  // (assert t) の t を受け取って ParikhConstraint の Seq を返す.
  private object BoolToConstraintTransformer extends TreeTransformer {

    type C = Unit

    type R = (ParikhConstraint, Seq[ParikhConstraint])

    override def combine(tree: Tree, context: C, results: Seq[R]): R = ???

  }
  private def boolToConstraint(term: SMTTerm): Seq[ParikhConstraint] = {
    val (_, (r, s)) = BoolToConstraintTransformer.transform(term, ())
    r +: s
  }

  def expandMacro(t: SMTTerm): SMTTerm = t match {
    case CoreTheory.Not(CoreTheory.Equals(SimpleQualID(s1), SimpleQualID(s2)))
        if env.get(s1).exists(_ == StringSort()) && env.get(s2).exists(_ == StringSort()) =>
      val (x, y) = (SimpleQualID(s1), SimpleQualID(s2))
      val i = SimpleQualID(freshTemp())
      // codeAt(x, i) != codeAt(y, i)
      CoreTheory.Not(CoreTheory.Equals(Strings.CodeAt(x, i), Strings.CodeAt(y, i)))
    case _ => t
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
      case Strings.Regex.Power(e, SNumeral(n)) =>
        val re = expectRegExp(e)
        (BigInt(1) to n).foldLeft(re) { case (acc, _) => CatExp(acc, re) }
      case Strings.Regex.Range(SString(c1), SString(c2)) if c1.length == 1 && c2.length == 1 =>
        throw new NotImplementedError("re.range is not implemented")
      case SimpleApp("re.comp", Seq(e)) => CompExp(expectRegExp(e))
      case Strings.Regex.AllChar()      => DotExp
      case SimpleQualID("re.all")       => StarExp(DotExp)
      case Strings.Regex.Parse(SString(s)) =>
        RegExpParser.parse(s).fold(
          errMsg => throw new Exception(s"re.parse: Cannot parse `$s': $errMsg"),
          identity
        )
      case _                            => throw new Exception(s"Cannot interpret given S-expression as regular expression: $t")
    }

  type SolverOption = Unit

  def expectPCRE(t: SMTTerm): PCRE[Char, Int] = {
    type RE = PCRE[Char, Int]
    var group = 0
    def nextGroup(): Int = {
      group += 1
      group
    }
    def aux(t: SMTTerm): RE = t match {
      case SimpleApp("str.to_pcre", Seq(SString(w))) =>
        w.map[RE](c => PCRE.Chars(Set(c)))
          .fold[PCRE[Char, Int]](PCRE.Eps())(PCRE.Cat.apply)
      case SimpleApp("pcre.alt", ts)   => ts.map(aux).reduce[RE](PCRE.Alt.apply)
      case SimpleApp("pcre.++", ts)    => ts.map(aux).reduce[RE](PCRE.Cat.apply)
      case SimpleApp("pcre.*", Seq(t)) => PCRE.Greedy(aux(t))
      case SimpleApp("pcre.+", Seq(t)) =>
        val pcre = aux(t)
        PCRE.Cat(pcre, PCRE.Greedy(pcre))
      case SimpleApp("pcre.*?", Seq(t)) => PCRE.NonGreedy(aux(t))
      case SimpleApp("pcre.group", Seq(t)) =>
        val group = nextGroup()
        PCRE.Group(aux(t), group)
      case SimpleQualID("pcre.allchar") => PCRE.AllChar()
      case _                            => throw new Exception(s"${t.getPos}: PCRE expected but found: $t")
    }
    aux(t)
  }

  def expectReplacement(t: SMTTerm): Replacement[Char, Int] = t match {
    case SimpleApp("pcre.replacement", ts) =>
      Replacement(
        ts.flatMap {
          case SString(w)            => w.map(Left.apply)
          case SNumeral(i) if i == 0 => Seq(Right(None))
          case SNumeral(i) if i > 0  => Seq(Right(Some(i.toInt)))
          case t                     => throw new Exception(s"${t.getPos}: PCRE Replacement component expected but found: $t")
        }
      )
    case _ => throw new Exception(s"${t.getPos}: PCRE Replacement expected but found: $t")
  }

  implicit def formula2constraint(f: Presburger.Formula[String]): ParikhConstraint = PureIntConstraint(f)

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
      case SimpleQualID(name) => (Presburger.Var(name), Seq.empty)
      case Ints.Neg(t) =>
        val (pt, cs) = expectInt(t)
        (Presburger.Const(0) - pt, cs)
      case Strings.Length(SimpleQualID(name)) =>
        val lenVar = provider.freshTemp()
        (Presburger.Var(lenVar), Seq(ParikhAssertion(name, ParikhLanguage.Length(lenVar))))
      case Strings.CountChar(SimpleQualID(name), SString(w)) if w.length == 1 =>
        val charNum = provider.freshTemp()
        (Presburger.Var(charNum), Seq(ParikhAssertion(name, ParikhLanguage.CountChar(charNum, w(0)))))
      case Strings.CodeAt(SimpleQualID(name), i) if env.get(name).exists(_ == StringSort()) =>
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
      case Strings.IndexOf(SimpleQualID(name), SString(w), SNumeral(c)) if c >= 0 =>
        val newVar = freshTemp()
        val constr = ParikhAssertion(name, ParikhLanguage.IndexOfConst(w, c.toInt, newVar))
        (Presburger.Var(newVar), Seq(constr))
      case Ints.Mul(SNumeral(c), t) =>
        val (pt, cs) = expectInt(t)
        (Presburger.Mult(Presburger.Const(c.toInt), pt), cs)
      case Strings.IndexOf(SimpleQualID(name), SString(w), t) =>
        val (i, cs) = parseAndAbstract(t)
        val j = freshTemp()
        (Presburger.Var(j), cs :+ ParikhAssertion(name, ParikhLanguage.IndexOf(w, i, j)))
      case _ =>
        throw new Exception(s"${t.optPos}: Cannot interpret given S-expression ${t} as int expression")
    }
  }

  // (substr x t1 t2) where t1 or t2 is not numeral
  def expectParikhTransduction(
      t: SMTTerm
  ): (String, ParikhTransduction[Char, String], Seq[ParikhConstraint]) = t match {
    case Strings.At(SimpleQualID(rhsVar), t) =>
      val (pt, cs) = expectInt(t)
      val (idx, len) = (freshTemp(), freshTemp())
      val intc = Seq[ParikhConstraint](
        pt === Presburger.Var(idx),
        Presburger.Const(1) === Presburger.Var(len)
      )
      (rhsVar, ParikhTransduction.Substr(idx, len), cs ++ intc)
    case Strings.Substring(SimpleQualID(rhsVar), t1, t2) =>
      val (pt1, cs1) = expectInt(t1)
      val (pt2, cs2) = expectInt(t2)
      val (from, len) = (freshTemp(), freshTemp())
      val intc = Seq[ParikhConstraint](pt1 === Presburger.Var(from), pt2 === Presburger.Var(len))
      (
        rhsVar,
        ParikhTransduction.Substr(from, len),
        cs1 ++ cs2 ++ intc
      )
    // TODO プリプロセス後は getPos できないので，他も修正
    // case _ => throw new Exception(s"${t.getPos}: Cannot interpret given S-expression ${t} as transduction")
    case _ => throw new Exception(s"Cannot interpret given S-expression ${t} as transduction")
  }

  object SimpleTransduction {
    // (rhs, transduction)
    def unapply(e: SMTTerm): Option[(String, Transduction[Char])] =
      e match {
        case Strings.Replace(SimpleQualID(name), SString(target), SString(word)) =>
          Some((name, Transduction.Replace(target, word)))
        case SimpleApp("str.replaceall", Seq(SimpleQualID(name), SString(target), SString(word))) =>
          Some((name, Transduction.ReplaceAll(target, word)))
        case SimpleApp("str.replace_all", Seq(SimpleQualID(name), SString(target), SString(word))) =>
          Some((name, Transduction.ReplaceAll(target, word)))
        case SimpleApp("str.replace_some", Seq(SimpleQualID(name), SString(target), SString(word))) =>
          Some((name, Transduction.ReplaceSome(target, word)))
        case Strings.At(SimpleQualID(name), SNumeral(pos)) =>
          Some((name, Transduction.At(pos.toInt)))
        case SimpleApp("str.replace_pcre", Seq(SimpleQualID(name), pcre, replacement)) =>
          Some((name, Transduction.ReplacePCRE(expectPCRE(pcre), expectReplacement(replacement))))
        case SimpleApp("str.replace_pcre_all", Seq(SimpleQualID(name), pcre, replacement)) =>
          Some((name, Transduction.ReplacePCREAll(expectPCRE(pcre), expectReplacement(replacement))))
        case SimpleApp("str.insert", Seq(SimpleQualID(name), SNumeral(pos), SString(word))) =>
          Some((name, Transduction.Insert(pos.toInt, word)))
        case SimpleApp("str.reverse", Seq(SimpleQualID(name))) =>
          Some((name, Transduction.Reverse()))
        case Strings.Substring(SimpleQualID(name), SNumeral(from), SNumeral(len)) =>
          Some((name, Transduction.Substr(from.toInt, len.toInt)))
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
    def unapply(t: SMTTerm): Option[(Presburger.Formula[String], Seq[ParikhConstraint])] = {
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

  object SString {
    private val unescape = PyExEscape.unescape _
    def unapply(term: Terms.SString): Option[String] =
      Terms.SString.unapply(term).map(unescape)
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
        case SimpleApp("str.insert", Seq(SimpleQualID(insertion), SimpleQualID(devided), t)) =>
          val (pt, cs) = expectInt(t)
          val idx = freshTemp()
          (
            InsertAssignment(name, insertion, devided, idx),
            cs :+ (Presburger.Eq(Presburger.Var(idx), pt))
          )
        case _ =>
          val (rhs, trans, cs) = expectParikhTransduction(t)
          (ParikhAssignment(name, trans, rhs), cs)
      }
    // Other equalities are between ints.
    case IntConstraint(f, cs) => (f, cs)
    case Strings.InRegex(SimpleQualID(name), t) =>
      val re = expectRegExp(t)
      (ParikhAssertion(name, ParikhLanguage.FromRegExp(re)), Seq.empty)
    case _ => throw new Exception(s"${if (t.hasPos) t.getPos else -1}: Unsupported assertion: ${t}")
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

  private def preprocess(commands: Seq[SMTCommands.Command]): Seq[SMTCommands.Command] = {
    val (res, varMap) = new Preprocessor(provider).preprocess(commands)
    userRepr = varMap
    res
  }

  def executeScript(script: SMTCommands.Script): Unit = {
    val cmds = preprocess(script.commands)
    cmds.foreach(c => logger.trace(c.toString))
    cmds.foreach(execute)
  }

  private def sortStringVars(constraints: Seq[ParikhConstraint]): Option[Seq[String]] = {
    // 重複する定義の存在を確認
    val dependers = constraints.flatMap(_.dependerVars)
    if (dependers.zipWithIndex.groupMap(_._1)(_._2).exists(_._2.length > 1))
      return None
    val dependees = constraints.flatMap(_.dependeeVars)
    val vars = (dependers.iterator ++ dependees).toSet
    val edges = for {
      c <- constraints
      er <- c.dependerVars
    } yield er -> c.dependeeVars.distinct
    val dependencyGraph = TopSort.Graph(vars, edges.toMap.withDefaultValue(Seq.empty))
    TopSort.sort(dependencyGraph).map { sorted =>
      // 代入文の左辺が後半に連続して現れるようにする
      // JSSST2021Strategy, PreImageStrategy はこの仮定に依存している
      val independent = vars -- dependers
      val (_, dependent) = sorted.partition(independent)
      independent.toSeq ++ dependent
    }
  }

  private def transform(
      constraints: Seq[ParikhConstraint],
      additionalAlphabet: Set[Char]
  ): Option[strategy.Input] = // SL でなければ None
    sortStringVars(constraints) map { stringVars =>
      val varIdx = stringVars.zipWithIndex.toMap
      val alphabet = {
        val used = constraints.flatMap(_.usedAlphabet).toSet
        used ++ additionalAlphabet
      }
      val assignments = constraints
        .collect { case a: AtomicAssignment[String] => a.renameVars(varIdx) }
        .sortBy(_.dependerVars.head)
      val assertions = constraints.collect { case a: ParikhAssertion[String] => a.renameVars(varIdx) }
      val arithFormula = constraints.collect { case PureIntConstraint(f)     => f }
      strategy.Input(
        alphabet,
        stringVars.length,
        assignments,
        assertions,
        arithFormula
      )
    }

}

trait Escape  {
  def unescape(w: String): String
}

// \f, \v, \r, \n, \t, \", \\, \xdd
// NOTE 最新仕様ではエスケープ文字ではないが，PyEx では "\x61" == "a" と考えている
//      Z3 でも 4.8.8 だとエスケープ文字扱いだが 4.11 では新しい仕様に基づき通常の文字扱いする
object PyExEscape extends Escape {
  private val hexCode = {
    val pat = raw"\\x([\dabcdef]{2})".r
    val fromHex = (s: String) => Integer.parseInt(s, 16).toChar.toString
    (w: String) => pat.replaceAllIn(w, m => fromHex(m.group(1)))
  }
  private val backslashesSeq: Seq[(Char, Char)] = Seq(
    ('f', '\f'),
    ('v', 11),
    ('r', '\r'),
    ('n', '\n'),
    ('t', '\t'),
    ('"', '"'),
    ('\\', '\\')
  )
  private val backslashes = backslashesSeq
    .map {
      case (target, replacement) =>
        val t = "\\" + target
        val r = replacement.toString
        (w: String) => w.replace(t, r)
    }
  private val func = (Seq(hexCode) ++ backslashes)
    .reduce[String => String] { case (acc, f) => acc andThen f }

  def unescape(w: String): String = func(w)
}

// \u{61} => a
object SMTLIBEscape extends Escape {
  private val pat = raw"\\u\{([\da-f]{1,5})\}".r
  private val fromHex = (s: String) => Integer.parseInt(s, 16).toChar.toString
  def apply(s: String): String = s.map(c => s"\\u{${c.toHexString}}").mkString
  // Java の Matcher#replaceAll は挙動がおかしいので \u{5c} に対して失敗する
  // (replacer の結果内の \ をエスケープ用プレフィックスと考える)
  def unescape(withEscape: String): String = pat.replaceAllIn(withEscape, m => fromHex(m.group(1)))
}
