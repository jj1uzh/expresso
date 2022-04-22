package com.github.kmn4.expresso.strategy

trait Strategy {
  // 最初の呼び出し時のみ計算を行う．
  // NOTE 異なる constraint で複数回呼び出した場合でも 2 度目以降には再計算を行わないことに注意．
  def checkSat(constraint: Input): Boolean

  def issat: Boolean

  // checkSat を呼び出した後でのみ正しい答えを出力する．
  // NOTE checkSat を一度も呼び出していない場合の動作は未定義である．
  def getModel(): Output

  def checkSatForall(v: String, constraint: Input): Boolean
}
