package com.github.kmn4.expresso.runner

import scala.util.Try

trait Runner {
  def check: Try[Unit]
  def run: Unit
}
