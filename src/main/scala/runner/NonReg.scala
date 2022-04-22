package com.github.kmn4.expresso.runner

import scala.collection.mutable
import scala.util.Try
import scala.util.chaining.scalaUtilChainingOps
import scala.util.control.Exception.nonFatalCatch
import scala.Console.{println => warn}
import com.github.kmn4.expresso.Solver
import com.github.kmn4.expresso.strategy.Strategy
import com.typesafe.scalalogging.Logger
import smtlib.trees.Commands
import smtlib.trees.Commands.{Assert, CheckSat, Command, DeclareConst, GetModel, Script}
import smtlib.trees.Terms
import smtlib.trees.Terms.{FunctionApplication, Identifier, Sort, SSymbol, Term}
import NonReg.NonRegScript
import com.github.kmn4.expresso.strategy.JSSST2021Strategy
import com.github.kmn4.expresso.strategy.PreImageStrategy
import smtlib.lexer.Tokens
import smtlib.printer.PrintingContext
import smtlib.trees.TreeTransformer
import scala.util.Success
import smtlib.trees.Terms.Let
import smtlib.trees.Terms.Forall
import smtlib.trees.Terms.Exists
import smtlib.trees.Terms.QualifiedIdentifier
import smtlib.trees.Terms.AnnotatedTerm
import smtlib.trees.Terms.SNumeral
import smtlib.trees.Terms.SHexadecimal
import smtlib.trees.Terms.SBinary
import smtlib.trees.Terms.SDecimal
import smtlib.trees.Terms.SString
import smtlib.trees.Terms.TermExtension

final case class NonReg(strategy: String, script: String)(implicit logger: Logger) extends Runner {

  //  private val alphabet = ('a' to 'c').toSet
  private val alphabet = ('0' to '1').toSet
  private def checker: Strategy =
    strategy match {
      case "jssst"    => new JSSST2021Strategy()
      case "preimage" => new PreImageStrategy()
      case other      => throw new Exception(s"Unknown strategy `$other`, known values: issst, preimage")
    }
  private def mkSolver: Solver = new Solver(checker, false, alphabet, () => checker)
  private def mkJSSSTSolver: Solver = new Solver(new JSSST2021Strategy(), false, alphabet, () => new JSSST2021Strategy())
  override def check: Try[Unit] = Success(())

  private case class Phase[T](verb: String)(body: NonRegScript => Either[String, T]) {
    def apply(script: NonRegScript): Either[String, T] = {
      println(s"==> $verb")
      body(script) tap {
        case Right(_) => println("  -> OK")
        case Left(_)  => println("  -> Failed")
      }
    }
  }

  override def run: Unit = {
    val originalIsSat = Phase("Checks whether original constraint is SAT") { script =>
      val originalScript = script.originalScript
//      print(originalScript.commands.mkString("|Original Constraints:\n|\t", "|\t", ""))
      val solver = mkSolver
      solver.executeScript(originalScript)
      if (solver.checkSat())
        Right(())
      else
        Left("Original constraint is UNSAT")
    }
    val nonPumpingSubsetIsInfinite = Phase("Checks whether the subset is infinite forall `p`") { script =>
      val solver = mkJSSSTSolver
      solver.executeScript(script.subsetScript)
      if (solver.checkSatForall(script.subset.vars.p.name))
        Right(())
      else
        Left("The subset using `p` is finite")
    }
    val evidenceIsNecessaryCondition =
      Phase("Checks whether the evidence constraints is necessary condition of original language") { script =>
        script.evidence.keysIterator.foldLeft[Either[String, Unit]](Right(())) {
          case (failure @ Left(_), _) => failure
          case (Right(_), key) =>
            val solver = mkSolver
            solver.executeScript(script.evidenceCheckScripts(key))
            if (solver.checkSat()) {
              val model = solver.getModel().get
              println("Model:")
              println(
                model._1.toVector.sorted.map { case (k, v) => s"$k -> '$v'" }.mkString("|\t", "\n|\t", "")
              )
              println(
                model._2.toVector.sorted.map { case (k, v) => s"$k -> '$v'" }.mkString("|\t", "\n|\t", "")
              )
              Left(s"Evidence for iteration $key is not a necessary condition of original language.")
              //              Left(s"Evidence for iteration is not a necessary condition of original language.")
            } else Right(())
        }
      }
    val nonRegScriptIsUnsat = Phase("Checks whether the variable is not regular...") { script =>
      val cmds = script.allScript.commands
//      print(cmds.mkString("|ALL Constraints:\n|\t", "|\t", ""))
      val solver = mkSolver
      solver.executeScript(Script(cmds))
      if (solver.checkSat()) {
        val model = solver.getModel().get
        println("Model:")
        println(model._1.toVector.sorted.map { case (k, v) => s"$k -> '$v'" }.mkString("|\t", "\n|\t", ""))
        println(model._2.toVector.sorted.map { case (k, v) => s"$k -> '$v'" }.mkString("|\t", "\n|\t", ""))
        Right(false)
      } else Right(true)
    }
    (for (
      s <- NonRegScript(script);
      _ <- originalIsSat(s);
      _ <- nonPumpingSubsetIsInfinite(s);
      _ <- evidenceIsNecessaryCondition(s);
      res <- nonRegScriptIsUnsat(s)
    ) yield res) match {
      case Left(msg) => /*Console.err.*/ println(s"Execution failed: $msg")
      case Right(res) =>
        println(
          if (res) "The variable is not regular"
          else "I don't know whether the variable is not regular"
        )
    }
  }
}

object NonReg {
  import NonRegScript.ExtCommands

  /** nonregを表現するスクリプト */
  case class NonRegScript private (
      original: ExtCommands.Original,
      subset: ExtCommands.Subset,
      split: ExtCommands.Split,
      evidence: Map[Int, ExtCommands.Evidence]
  ) {
    import NonRegScript._
    private def stringSort = Sort(Identifier(SSymbol("String")))
    private def intSort = Sort(Identifier(SSymbol("Int")))
    private def fn(name: String, args: Term*) =
      Terms.FunctionApplication(Terms.QualifiedIdentifier(Terms.Identifier(SSymbol(name))), args)
    private implicit def term(sym: SSymbol): Term =
      Terms.QualifiedIdentifier(Terms.Identifier(sym))
    private implicit def snumeral(n: Int): Term =
      Terms.SNumeral(n)
    def originalScript: Script = Script(
      DeclareConst(original.vars.w, stringSort) +:
        original.constraints
    )
    def subsetScript: Script = {
      val subsetVarDeclare =
        if (original.vars.w == subset.vars.subset) Nil else List(DeclareConst(subset.vars.subset, stringSort))
      Script( // ここでは (declare-const p Int) は入れない．Forallするため
        originalScript.commands ++
          subsetVarDeclare ++
          List(Assert(fn("<=", subset.vars.p, fn("str.len", subset.vars.subset)))) ++
          subset.constraints
      )
    }
    private def splitCmds: List[Command] = {
      val SplitVars(i, j, x, y, z) = split.vars
      val SubsetVars(p, s) = subset.vars
      List(
        DeclareConst(i, intSort),
        DeclareConst(j, intSort),
        DeclareConst(x, stringSort),
        DeclareConst(y, stringSort),
        DeclareConst(z, stringSort),
        Assert(fn(">=", i, 0)),
        Assert(fn(">", j, 0)),
        Assert(fn("<=", fn("+", i, j), p)),
        Assert(fn("=", x, fn("str.substr", s, 0, i))),
        Assert(fn("=", y, fn("str.substr", s, i, j))),
        Assert(fn("=", z, fn("str.substr", s, fn("+", i, j), fn("-", fn("-", fn("str.len", s), i), j))))
      )
    }

    def evidenceCheckScripts: Map[Int, Script] =
      evidence.view.mapValues { case ExtCommands.Evidence(_, EvidenceVars(wt), cmds) =>
        val originalTargetVarSymbol = original.vars.w
        val w = Identifier(originalTargetVarSymbol)
        val declareConstCmds = cmds.collect { case c: DeclareConst => c }
        def rename(t: Term): Term = (t: @unchecked) match { // 繰り返し文字列 wt を 元の制約の w に置き換える
          case QualifiedIdentifier(id, sort) if id.symbol == wt => QualifiedIdentifier(w, sort)
          case AnnotatedTerm(term, attr, attrs)                 => AnnotatedTerm(rename(term), attr, attrs)
          case FunctionApplication(fun, terms)                  => FunctionApplication(fun, terms.map(rename))
          case other                                            => other
        }
        val assertCmds = cmds.collect { case Assert(t) => Assert(rename(t)) }
        val assertNotTerm = cmds.collect { case ExtCommands.AssertEvidence(t) => rename(t) }.reduceRight { fn("and", _, _) }
        Script(
          List(DeclareConst(original.vars.w, stringSort)) ++
            originalScript.commands ++
            declareConstCmds ++
            assertCmds ++
            List(Assert(fn("not", assertNotTerm)))
        )
      }.toMap

    private def evidenceCmds: List[Command] = {
      val SplitVars(_, _, x, y, z) = split.vars
      evidence.flatMap { case (count, ExtCommands.Evidence(_, EvidenceVars(wt), cmds)) =>
        DeclareConst(wt, stringSort) ::
          Assert(fn("=", wt, fn("str.++", (x +: Seq.fill(count)(y) :+ z).map(term): _*))) ::
          cmds.map { case ExtCommands.AssertEvidence(t) => Assert(t); case other => other }
      }.toList
    }
    def allScript: Script = Script(
      List(DeclareConst(subset.vars.p, intSort)) ++
        subsetScript.commands ++
        List(Assert(fn(">", subset.vars.p, 0))) ++
        splitCmds ++
        evidenceCmds
    )
  }
  object NonRegScript {
    final case class OriginalVars(w: SSymbol)
    final case class SubsetVars(p: SSymbol, subset: SSymbol)
    final case class SplitVars(i: SSymbol, j: SSymbol, x: SSymbol, y: SSymbol, z: SSymbol)
    final case class EvidenceVars(wt: SSymbol)

    /** スクリプト文字列をnonregスクリプトとして解釈する．不正なスクリプトであればLeft */
    def apply(scriptStr: String): Either[String, NonRegScript] = {
      import ExtCommands._
      val lexer = new ExtLexer(new java.io.StringReader(scriptStr))
      val parser = new ExtParser(lexer)
      val cmds = parser.parseScript.commands
      nonFatalCatch
        .either {
          cmds match {
            case List(o: Original, sb: Subset, sp: Split, rest @ _*) =>
              val ev = rest.map {
                case cmd @ Evidence(count, _, _) => count -> cmd
                case cmd =>
                  throw new Exception(s"${cmd.getPos}: expected nonreg/evidence command but found ${cmd}")
              }.toMap
              NonRegScript(o, sb, sp, ev)
          }
        }
        .left
        .map(_.getMessage)
    }
    object ExtCommands {
      final case class Original(vars: OriginalVars, constraints: List[Command])
          extends Commands.CommandExtension {
        override def print(ctx: PrintingContext): Unit = {
          ctx.print("(nonreg/original ")
          ctx.print(vars.w)
          ctx.printNary(constraints, " \n\t", "\n\t", ")")
        }
        override def transform(tt: TreeTransformer)(context: tt.C): (Command, tt.R) = ???
      }
      final case class Subset(vars: SubsetVars, constraints: List[Command])
          extends Commands.CommandExtension {
        override def print(ctx: PrintingContext): Unit = {
          ctx.print("(nonreg/subset ")
          ctx.printNary(List(vars.p, vars.subset), "", " ", "")
          ctx.printNary(constraints, " \n\t", "\n\t", ")")
        }
        override def transform(tt: TreeTransformer)(context: tt.C): (Command, tt.R) = ???
      }
      final case class Split(vars: SplitVars) extends Commands.CommandExtension {
        override def print(ctx: PrintingContext): Unit = {
          ctx.print("(nonreg/split ")
          ctx.printNary(List(vars.i, vars.j, vars.x, vars.y, vars.z), "", " ", "")
        }
        override def transform(tt: TreeTransformer)(context: tt.C): (Command, tt.R) = ???
      }
      final case class Evidence(count: Int, vars: EvidenceVars, constraints: List[Command])
          extends Commands.CommandExtension {
        override def print(ctx: PrintingContext): Unit = {
          ctx.print(s"(nonreg/evidence $count ")
          ctx.print(vars.wt)
          ctx.printNary(constraints, " \n\t", "\n\t", "")
        }
        override def transform(tt: TreeTransformer)(context: tt.C): (Command, tt.R) = ???
      }
      final case class AssertEvidence(t: Term) extends Commands.CommandExtension {
        override def print(ctx: PrintingContext): Unit = {
          ctx.print(s"(assert-evidence $t)")
        }
        override def transform(tt: TreeTransformer)(context: tt.C): (Command, tt.R) = {
          val (rt, rc) = tt.transform(t, context)
          (AssertEvidence(rt), tt.combine(this, context, Seq(rc)))
        }
      }
    }
    private object ExtTokens {
      import Tokens.ReservedWord
      case object Original extends ReservedWord
      case object Subset extends ReservedWord
      case object Split extends ReservedWord
      case object Iter extends ReservedWord
      case object Evidence extends ReservedWord
      case object AssertEvidence extends ReservedWord
    }
    import ExtTokens._
    private class ExtLexer(reader: java.io.Reader) extends smtlib.lexer.Lexer(reader) {
      import Function1.UnliftOps
      private val toReservedFn: String => Option[Tokens.Token] =
        Map(
          "original" -> Original,
          "subset" -> Subset,
          "split" -> Split,
          "iter" -> Iter,
          "evidence" -> Evidence
        )
          .map { case (k, v) => s"nonreg/$k" -> Tokens.Token(v) }
          .orElse[String, Tokens.Token] {
            case "assert-evidence" => Tokens.Token(AssertEvidence)
          }
          .orElse((super.toReserved _).unlift)
          .lift
      override protected def toReserved(s: String): Option[Tokens.Token] = toReservedFn(s)
    }
    private class ExtParser(lexer: smtlib.lexer.Lexer) extends smtlib.parser.Parser(lexer) {
      private def parsePrognWithoutParen: List[Command] = {
        LazyList
          .continually(if (peekToken.kind != Tokens.CParen) parseCommand else null)
          .takeWhile(_ != null)
          .toList
      }
      override protected def parseCommandWithoutParens: Command = getPeekToken.kind match {
        case Original =>
          eat(Original)
          val vars = OriginalVars(parseSymbol)
          ExtCommands.Original(vars, parsePrognWithoutParen)
        case Subset =>
          eat(Subset)
          val vars = SubsetVars(parseSymbol, parseSymbol)
          ExtCommands.Subset(vars, parsePrognWithoutParen)
        case Split =>
          eat(Split)
          val vars = SplitVars(parseSymbol, parseSymbol, parseSymbol, parseSymbol, parseSymbol)
          ExtCommands.Split(vars)
        case Evidence =>
          eat(Evidence)
          val count = parseNumeral.value.toInt
          val vars = EvidenceVars(parseSymbol)
          ExtCommands.Evidence(count, vars, parsePrognWithoutParen)
        case AssertEvidence =>
          eat(AssertEvidence)
          ExtCommands.AssertEvidence(parseTerm)
        case _ => super.parseCommandWithoutParens
      }
    }
  }
}
