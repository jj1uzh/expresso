package com.github.kmn4.expresso.language

import com.github.kmn4.expresso.math._

object Constraint {

  sealed trait ParikhConstraint[S] {
    def usedAlphabet: Set[Char]
    def dependerVars: Seq[S]
    def dependeeVars: Seq[S]
    // 文字列変数を付け替える
    def renameVars[T](f: S => T): ParikhConstraint[T]
  }
  sealed trait AtomicAssignment[S] extends ParikhConstraint[S] {
    def lhsStringVar: S

    override def dependerVars: Seq[S] = Seq(lhsStringVar)

    override def renameVars[T](f: S => T): AtomicAssignment[T]
  }
  case class ParikhAssignment[S](
      lhsStringVar: S,
      trans: ParikhTransduction[Char, String],
      rhsStringVar: S
  ) extends AtomicAssignment[S] {

    override def renameVars[T](f: S => T): AtomicAssignment[T] =
      copy(lhsStringVar = f(lhsStringVar), rhsStringVar = f(rhsStringVar))

    override def dependerVars: Seq[S] = Seq(lhsStringVar)

    override def dependeeVars: Seq[S] = Seq(rhsStringVar)

    override def usedAlphabet: Set[Char] = trans.usedAlphabet
  }
  // Left(word), Right(stringVar)
  case class CatAssignment[S](lhsStringVar: S, wordAndVars: Seq[Either[Seq[Char], S]])
      extends AtomicAssignment[S] {

    override def renameVars[T](f: S => T): AtomicAssignment[T] =
      copy(lhsStringVar = f(lhsStringVar), wordAndVars = wordAndVars.map(_.map(f)))

    override def dependerVars: Seq[S] = Seq(lhsStringVar)

    override def dependeeVars: Seq[S] = wordAndVars.flatMap(_.toOption)

    override def usedAlphabet: Set[Char] = wordAndVars.flatMap(_.left.getOrElse(Set.empty)).toSet
  }
  case class InsertAssignment[S](lhsStringVar: S, insertionVar: S, dividedVar: S, indexVar: String)
      extends AtomicAssignment[S] {
    override def renameVars[T](f: S => T): AtomicAssignment[T] =
      copy(lhsStringVar = f(lhsStringVar), insertionVar = f(insertionVar), dividedVar = f(dividedVar))
    override def dependerVars: Seq[S] = Seq(lhsStringVar)
    override def dependeeVars: Seq[S] = Seq(insertionVar, dividedVar)
    override def usedAlphabet: Set[Char] = Set.empty
  }

  sealed trait RegIntConstraint[S] extends ParikhConstraint[S]

  case class ParikhAssertion[S](stringVar: S, lang: ParikhLanguage[Char, String])
      extends ParikhConstraint[S]
      with RegIntConstraint[S] {

    override def dependerVars: Seq[S] = Seq.empty

    override def dependeeVars: Seq[S] = Seq(stringVar)

    override def usedAlphabet: Set[Char] = lang.usedAlphabet

    override def renameVars[T](f: S => T): ParikhAssertion[T] = copy(stringVar = f(stringVar))
  }
  // type PureIntConstraint = Presburger.Formula[String]
  case class PureIntConstraint[S](val formula: Presburger.Formula[String])
      extends ParikhConstraint[S]
      with RegIntConstraint[S] {

    override def dependerVars: Seq[S] = Seq.empty

    override def dependeeVars: Seq[S] = Seq.empty

    override def usedAlphabet: Set[Char] = Set.empty

    override def renameVars[T](f: S => T): PureIntConstraint[T] =
      new PureIntConstraint(formula)
  }

  case class DisjunctionConstraint[S](passertion: ParikhAssertion[S], intc: PureIntConstraint[S])
      extends ParikhConstraint[S] {
    override def usedAlphabet: Set[Char] = passertion.usedAlphabet ++ intc.usedAlphabet
    override def dependerVars: Seq[S] = passertion.dependerVars ++ intc.dependerVars
    override def dependeeVars: Seq[S] = passertion.dependeeVars ++ intc.dependeeVars
    override def renameVars[T](f: S => T): DisjunctionConstraint[T] =
      DisjunctionConstraint(passertion.renameVars(f), intc.renameVars(f))
  }
}
