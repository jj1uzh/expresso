package com.github.kmn4.expresso.runner

import com.github.kmn4.expresso.Solver
import com.github.kmn4.expresso.strategy.Strategy
import smtlib.trees.Commands.Script
import scala.util.{Success, Try}

final case class Solve(strategy: Strategy, script: Script) extends Runner {

  private def alphabet = ('a' to 'c').toSet
//  private def alphabet = ('!' to '~').toSet
//  private def alphabet = (0 to 127).toSet

  def check: Try[Unit] = {
    Success(())
  }

  def run: Unit = {
    val solver = new Solver(strategy, print = true, alphabet)
    solver.executeScript(script)
  }
}
