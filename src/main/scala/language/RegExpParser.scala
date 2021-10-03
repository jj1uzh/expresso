package com.github.kmn4.expresso.language

import scala.util.parsing.combinator.RegexParsers
import com.github.kmn4.expresso.language.RegExp.{EpsExp, CatExp, CharExp, OrExp, StarExp}

private[expresso] object RegExpParser {

  /** Parse regexp expression.
    * @return Right if parsed successfully, otherwise Left with error message. */
  def parse(s: String): Either[String, RegExp[Char]] =
    Parsers.parseAll(Parsers.all, s) match {
      case Parsers.NoSuccess(msg, _)  => Left(msg)
      case Parsers.Success(result, _) => Right(result)
    }

  private object Parsers extends RegexParsers {
    def all: Parser[RegExp[Char]] =
      union | cat | empty

    def cat: Parser[RegExp[Char]] =
      ((star | single | paren).+ ^^ (_ reduceRight CatExp.apply))

    def paren: Parser[RegExp[Char]] =
      "(" ~ all ~ ")" ^^ { case _ ~ r ~ _ => r }

    def single: Parser[CharExp[Char, Char]] =
      "\\" ~ ".".r ^^ { case _ ~ mch => CharExp(mch.toString charAt 0) } |
        "[^|()*]".r ^^ { mch => CharExp(mch.toString charAt 0) }

    def star: Parser[StarExp[Char, Char]] =
      single ~ "*" ^^ { case s ~ _           => StarExp(s) } |
        "(" ~ all ~ ")*" ^^ { case _ ~ r ~ _ => StarExp(r) }

    def union: Parser[OrExp[Char, Char]] =
      (cat | empty) ~ "|" ~ all ^^ { case r1 ~ _ ~ r2 => OrExp(r1, r2) }

    def empty: Parser[EpsExp.type] =
      "" ^^ (_ => EpsExp)
  }
}
