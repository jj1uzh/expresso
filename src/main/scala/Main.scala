package com.github.kmn4.expresso

import com.github.kmn4.expresso.strategy.PreImageStrategy

import java.nio.file.FileSystems
import scala.util.control.Exception.nonFatalCatch
import com.typesafe.scalalogging.Logger
import com.github.kmn4.expresso.strategy.JSSST2021Strategy

// command line arguments:
// - 0 : constraint file name (.smt2 file)
// - 1 : solving strategy, either 'preimage' or 'jssst' (defaults to 'jssst')
object Main extends App {
  val appname = "expresso"
  case class AppException(msg: String) extends Throwable
  try {
    val Array(fnameOpt, strategy) = args.map(Option(_)).padTo(2, None)
    val fname = fnameOpt getOrElse (throw AppException("Constraint file not specified"))
    implicit val logger = Logger(s"bench.${fname.split('/').last.split('.').dropRight(1).mkString}")
    val path = FileSystems.getDefault().getPath(fname)
    val file = path.toFile()
    if (!file.exists) throw AppException(s"Constraint file not found: ${path}")
    val lexer = new smtlib.lexer.Lexer(new java.io.FileReader(file))
    val parser = new smtlib.parser.Parser(lexer)
    val script =
      nonFatalCatch
        .withApply(err => throw AppException(s"Failed to parse ${fname}: ${err.getMessage}"))
        .apply { parser.parseScript }
    val checker = strategy getOrElse "jssst" match {
      case "preimage" => new PreImageStrategy
      case "jssst"    => new JSSST2021Strategy
      case s          => throw AppException(s"Invalid strategy: ${s}\nPossible values are: preimage, jssst")
    }

    // alpahbet to be added
    val alphabet = ('a' to 'c').toSet
    //  val alphabet = ('!' to '~').toSet
    //  val alphabet = (0 to 127).toSet

    new Solver(
      checker = checker,
      print = true,
      alphabet = alphabet
    ).executeScript(script)
  } catch {
    case AppException(msg) => Console.err.println(s"$appname: $msg")
  }
}
