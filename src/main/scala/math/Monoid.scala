package com.github.kmn4.expresso.math

trait Monoid[M] {
  def unit: M
  def combine(m1: M, m2: M): M
}

object Monoid {
  implicit def seqMonoid[A] = new Monoid[Seq[A]] {
    def unit = Nil
    def combine(l1: Seq[A], l2: Seq[A]) = l1 ++ l2
  }
  implicit def unitMonoid = new Monoid[Unit] {
    def unit = ()
    def combine(u1: Unit, u2: Unit) = ()
  }
  implicit def vectorMonoid[K, V: Monoid](ks: Iterable[K]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    val unit = ks.map(_ -> Monoid[V].unit).toMap
    def combine(v1: Map[K, V], v2: Map[K, V]) = {
      //      ks.map(k => k -> Monoid[V].combine(v1(k), v2(k))).toMap
      val m = ks.map(k => k -> Monoid[V].combine(v1.getOrElse(k, Monoid[V].unit), v2.getOrElse(k, Monoid[V].unit))).toMap
      m
    }
  }
  implicit val intAdditiveMonoid: Monoid[Int] = new Monoid[Int] {
    def unit = 0
    def combine(i1: Int, i2: Int) = i1 + i2
  }
  implicit def setUnionMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def unit: Set[A] = Set.empty
    def combine(m1: Set[A], m2: Set[A]): Set[A] = m1 union m2
  }
  implicit def productMonoid[M1: Monoid, M2: Monoid]: Monoid[(M1, M2)] = new Monoid[(M1, M2)] {
    def unit: (M1, M2) = (Monoid[M1].unit, Monoid[M2].unit)
    def combine(m1: (M1, M2), m2: (M1, M2)): (M1, M2) = (m1, m2) match {
      case ((m11, m12), (m21, m22)) => (Monoid[M1].combine(m11, m21), Monoid[M2].combine(m12, m22))
    }
  }

  def apply[M: Monoid]: Monoid[M] = implicitly[Monoid[M]]

  // TODO Set can be arbitrary monad. This is a special case of WriterT.
  def transition[Q, A, M](
      qs: Set[Q],
      w: Seq[A],
      delta: (Q, A) => Set[(Q, M)]
  )(
      implicit monoid: Monoid[M]
  ): Set[(Q, M)] =
    w.foldLeft(qs.map((_, monoid.unit))) {
      case (configs, a) =>
        configs.flatMap {
          case (q, m1) => delta(q, a).map { case (q, m2) => (q, monoid.combine(m1, m2)) }
        }
    }
  def fold[M](ms: IterableOnce[M])(implicit monoid: Monoid[M]): M = ms.iterator.fold(monoid.unit)(monoid.combine)
}
