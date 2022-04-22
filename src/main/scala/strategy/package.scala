package com.github.kmn4.expresso

import com.github.kmn4.expresso.math.Presburger
import com.github.kmn4.expresso.language.Constraint

/** 制約を解くアルゴリズムを表現するクラス Strategy を提供する．
  *
  * 現在は JSSST 2021 予稿で示したアルゴリズム JSSST2021Strategy と 逆像計算に基づく PreImageStrategy の 2 つを提供している．
  *
  * アルゴリズムの入力はペア (アルファベット, 文字列変数の集合，代入文の列，言語制約の集合，整数制約)． より詳しく，
  *   - アルファベットは文字の集合
  *   - 文字列変数の集合は {0, 1, ..., n-1} で，具体的には n が与えられる
  *   - 代入文は (左辺の文字列変数, PSST を構成するための構造, 右辺の文字列変数の列)
  *   - 言語制約は (文字列変数，PA を構成するための構造)
  *   - 整数制約は整数変数のみを含む線形算術論理式 を受け取る．
  *
  * 出力は充足可能かどうかの情報と，(sat ならば) 文字列変数と整数変数への値の割り当てとのペアである．
  */
package object strategy {
  case class Input(
      alphabet: Set[Char],
      stringVarNumber: Int,
      assignments: Seq[Constraint.AtomicAssignment[Int]],
      assertions: Seq[Constraint.ParikhAssertion[Int]],
      arithFormulas: Seq[Presburger.Formula[String]],
      disjAssertions: Seq[Constraint.DisjunctionConstraint[Int]] = Nil
  )

  type Models = (Seq[String], Map[String, Int])
  type Output = Option[Models]

  private[strategy] type Formulas[I, L] = Seq[Presburger.Formula[Either[I, L]]]
  private[strategy] type GlobalFormulas[I] = Seq[Presburger.Formula[I]]
  private[strategy] type PreImage[Q, A, L, I] = Iterator[ParikhRelation[Q, A, L, I]]

}
