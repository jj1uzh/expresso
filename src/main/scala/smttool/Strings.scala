package com.github.kmn4.expresso.smttool

import smtlib.theories.experimental.{Strings => ExpStrings}
import smtlib.theories.Operations
import smtlib.trees.Terms.{Term, FunctionApplication, QualifiedIdentifier, Identifier, SSymbol}
import smtlib.trees.Terms

object Strings {
  val StringSort = ExpStrings.StringSort

  val Contains = ExpStrings.Experimental.Contains
  val ToRegex = ExpStrings.ToRegex
  val At = ExpStrings.At
  val Substring = ExpStrings.Substring
  val Replace = ExpStrings.Experimental.Replace
  val Concat = ExpStrings.Concat
  val IndexOf = ExpStrings.Experimental.IndexOf
  val Length = ExpStrings.Length
  val InRegex = ExpStrings.InRegex

  object PrefixOf extends Operations.Operation2 {
    override val name: String = "str.prefixof"
  }

  object ReplaceAll extends Operations.Operation3 {
    override val name: String = "str.replace_all"
  }

  object Regex {
    val Concat = ExpStrings.Regex.Concat
    val AllChar = ExpStrings.Regex.AllChar
    val * = ExpStrings.Regex.*
    val + = ExpStrings.Regex.+
    val Union = ExpStrings.Regex.Union
    val Range = ExpStrings.Regex.Range

    object Comp extends Operations.Operation1 {
      override val name: String = "re.comp"
    }

    object All extends Operations.Operation0 {
      override val name: String = "re.all"
    }

    private val RegexPower = "re.^"
    // Power(r, rep) == ((_ re.^ rep), r)
    object Power {
      def apply(r: Term, repetition: Terms.SExpr): Term =
        FunctionApplication(
          QualifiedIdentifier(Identifier(SSymbol(RegexPower), Seq(repetition))),
          Seq(r)
        )
      def unapply(term: Term): Option[(Term, Terms.SExpr)] = term match {
        case FunctionApplication(
            QualifiedIdentifier(Identifier(SSymbol(RegexPower), Seq(repetition)), None),
            Seq(r)
            ) =>
          Some((r, repetition))
        case _ => None
      }
    }

    // 独自拡張
    object Parse extends Operations.Operation1 {
      val name = "re.parse"
    }
  }

  // 独自拡張
  object CountChar extends Operations.Operation2 {
    override val name: String = "str.count_char"
  }

  object CodeAt extends Operations.Operation2 {
    override val name: String = "str.code_at"
  }

}
