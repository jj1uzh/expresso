package com.github.kmn4.expresso

import com.github.kmn4.expresso.strategy.PreImageStrategy

import java.nio.file.FileSystems
import scala.util.control.Exception.nonFatalCatch
import com.typesafe.scalalogging.Logger
import com.github.kmn4.expresso.strategy.JSSST2021Strategy
import com.github.kmn4.expresso.runner.{NonReg, Runner, Solve}
import scala.collection.mutable.Buffer
import org.slf4j.LoggerFactory
import scala.io.Source

// command line arguments:
// - 0 : constraint file name (.smt2 file)
// - 1 : solving strategy, either 'preimage' or 'jssst' (defaults to 'jssst')
object Main extends App {
  val appname = "expresso"
  def usage: String = s"Usage: expresso [--nonreg] CONSTRAINT_FILE {preimage | jssst}"
  val margs = args.toBuffer
  val mode = margs.indexOf("--nonreg") match {
    case i if i >= 0 => margs.remove(i); "nonreg"
    case _           => "solve"
  }

  case class AppException(msg: String) extends Throwable
  try {
    val Buffer(fnameOpt, strategy) = margs.map(Option(_)).padTo(2, None)
    val fname = fnameOpt getOrElse (throw AppException(s"Constraint file not specified\n$usage"))
    implicit val logger = Logger(s"bench.${fname.split('/').last.split('.').dropRight(1).mkString}")
    val path = FileSystems.getDefault().getPath(fname)
    val file = path.toFile()
    if (!file.exists) throw AppException(s"Constraint file not found: ${path}")

    // alpahbet to be added
    val alphabet = ('0' to '1').toSet
    //  val alphabet = ('!' to '~').toSet
    //  val alphabet = (0 to 127).toSet

    //    val runner: Runner = NonReg(checker, script)
    val runner: Runner =
      if (mode == "nonreg")
        NonReg(strategy getOrElse "jssst", Source.fromFile(file).mkString)
      else {
        val lexer = new smtlib.lexer.Lexer(new java.io.FileReader(file))
        val parser = new smtlib.parser.Parser(lexer)
        val script =
          nonFatalCatch
            .withApply(err => throw AppException(s"Failed to parse ${fname}: ${err.getMessage}"))
            .apply { parser.parseScript }
        val checker = strategy getOrElse "jssst" match {
          case "preimage" => new PreImageStrategy
          case "jssst"    => new JSSST2021Strategy
          case s => throw AppException(s"Invalid strategy: ${s}\nPossible values are: preimage, jssst")
        }
        Solve(checker, script)
      }
    runner.check.fold(
      err => throw err,
      _ => runner.run
    )
  } catch {
    case AppException(msg) => Console.err.println(s"$appname: error: $msg")
  }
}
