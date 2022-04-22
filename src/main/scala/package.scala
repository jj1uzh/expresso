package com.github.kmn4

import com.github.kmn4.expresso.math.{Cop, Cop1, Cop2, Monoid}
import scala.collection.immutable.Queue
import smtlib.trees.Commands
import smtlib.trees.Terms
import smtlib.printer.PrintingContext
import smtlib.trees.TreeTransformer

package object expresso {

  type SeqEnv[X, B] = Map[X, Seq[B]]
  type Cupstar[X, B] = List[Cop[X, B]]
  type Update[X, B] = Map[X, Cupstar[X, B]]

  def emptyEnv[X, B]: SeqEnv[X, B] = Map.empty[X, Seq[B]].withDefaultValue(Seq.empty)

  implicit class SeqEnvOps[X, B](env: SeqEnv[X, B]) {
    def eval(xbs: Cupstar[X, B]): List[B] =
      xbs.flatMap {
        case Cop1(x) => env(x)
        case Cop2(b) => Seq(b)
      }.toList
  }

  object Cupstar {
    def devideChars[X, A, B](xbs: Cupstar[X, (A, B)]): (Cupstar[X, A], Cupstar[X, B]) =
      xbs.foldRight[(Cupstar[X, A], Cupstar[X, B])]((List.empty, List.empty)) {
        case (x @ Cop1(_), (a1, a2))  => (x.copy :: a1, x.copy :: a2)
        case (Cop2((a, b)), (a1, a2)) => (Cop2(a) :: a1, Cop2(b) :: a2)
      }
  }

  object Update {
    def devideChars[X, A, B](update: Update[X, (A, B)]): (Update[X, A], Update[X, B]) = {
      val aBuilder = Map.newBuilder[X, Cupstar[X, A]]
      val bBuilder = Map.newBuilder[X, Cupstar[X, B]]
      update.map {
        case (x, xabs) =>
          val (xas, xbs) = Cupstar.devideChars(xabs)
          aBuilder.addOne((x, xas))
          bBuilder.addOne((x, xbs))
      }
      (aBuilder.result(), bBuilder.result())
    }
  }

  implicit class UpdateOps[X, B](m: Update[X, B]) {
    def subst(xbs: List[Cop[X, B]]): List[Cop[X, B]] = xbs.flatMap {
      case Cop1(x) => m(x)
      case Cop2(b) => List(Cop2(b))
    }
    def update(env: SeqEnv[X, B]): SeqEnv[X, B] = m.map { case (x, xbs) => x -> env.eval(xbs) }
  }

  def flatMap1[A, B, C](abs: Cupstar[A, B], f: A => Cupstar[C, B]): Cupstar[C, B] =
    abs.flatMap { case Cop1(a) => f(a); case Cop2(b) => List(Cop2(b)) }
  def erase1[A, B](abs: Cupstar[A, B]): List[B] = abs.flatMap(Cop.erase1(_))
  def erase2[A, B](abs: Cupstar[A, B]): List[A] = abs.flatMap(Cop.erase2(_))
  def varsIn[X, A](xas: Cupstar[X, A]): Set[X] = erase2(xas).toSet
  def charsIn[X, A](xas: Cupstar[X, A]): Set[A] = erase1(xas).toSet
  def charsIn[X, A](m: Update[X, A]): Set[A] = m.flatMap { case (_, xas) => charsIn(xas) }.toSet
  implicit def updateMonoid[X, B](xs: Iterable[X]): Monoid[Update[X, B]] =
    new Monoid[Update[X, B]] {
      def combine(m1: Update[X, B], m2: Update[X, B]): Update[X, B] = Map.from {
        for (x <- xs) yield (x -> flatMap1(m2(x), m1(_)))
      }
      // Some codes assume that updates contain definition for all variables,
      // so cannot use `Map.empty.withDefault(x => x -> List(Cop1(x)))` as `unit`.
      def unit: Update[X, B] = Map.from(xs.map(x => x -> List(Cop1(x))))
    }

  def closure[Q](start: Set[Q], edges: Q => Set[Q]): Set[Q] = {
    def trans(qs: Set[Q]): Set[Q] =
      qs.foldLeft(Set.empty[Q]) { case (acc, q) => acc union edges(q) }
    var newQs = start
    var clos = start
    while (newQs.nonEmpty) {
      newQs = trans(newQs) -- clos
      clos ++= newQs
    }
    clos
  }

  def searchStates[Q, A, C, E](
      baseStates: Set[Q],
      inSet: Set[A]
  )(nextConfigs: (Q, A) => IterableOnce[C])(toState: C => Q, toEdge: (Q, A, C) => E): (Set[Q], Set[E]) = {
    val states = collection.mutable.Set.from(baseStates)
    var edges: List[E] = Nil
    var stack: List[Q] = states.toList
    while (stack.nonEmpty) {
      val h = stack.head
      stack = stack.tail
      for {
        a <- inSet
        c <- nextConfigs(h, a).iterator
      } {
        edges ::= toEdge(h, a, c)
        val q = toState(c)
        if (states.add(q)) {
          stack ::= q
        }
      }
    }
    (states.toSet, edges.toSet)
  }

  def searchStatesInt[Q, A, C, E](
      baseStates: Set[Q],
      inSet: Set[A]
  )(
      nextConfigs: (Q, A) => IterableOnce[C]
  )(toState: C => Q, toEdge: (Q, A, C, collection.Map[Q, Int]) => E): (collection.Map[Q, Int], List[E]) = {
    val states = collection.mutable.Map.empty[Q, Int]
    val newState: Unit => Int = {
      var q = -1
      _ => q += 1; q
    }
    for (q <- baseStates) states(q) = newState(())
    var edges: List[E] = Nil
    var stack: List[Q] = baseStates.toList
    while (stack.nonEmpty) {
      val h = stack.head
      stack = stack.tail
      for {
        a <- inSet
        c <- nextConfigs(h, a).iterator
      } {
        val q = toState(c)
        states.get(q) match {
          case Some(_) =>
          case None =>
            states(q) = newState(())
            stack ::= q
        }
        edges ::= toEdge(h, a, c, states)
      }
    }
    (states, edges)
  }

  /** Breadth-first search for an input by which given system can transition to final state. */
  def transitionSystemBFS[Q, A](
      states: Set[Q],
      in: Iterable[A],
      trans: (Q, A) => Set[Q],
      q0: Q,
      finals: Set[Q]
  ): List[A] = {
    var visited: Set[Q] = Set.empty
    var toVisit: Queue[(Q, List[A])] = Queue((q0, Nil))
    while (toVisit.nonEmpty) {
      val (q, as) = toVisit.head
      toVisit = toVisit.tail
      if (finals contains q) return as.reverse
      val fromQ = in.flatMap(a => trans(q, a).map((_, a :: as))).toSet
      val notVisited = fromQ.filterNot(visited contains _._1)
      visited ++= notVisited.map(_._1)
      toVisit = toVisit.appendedAll(notVisited)
    }
    throw new Exception("Given system is empty.")
  }

  def wrapSome[T](s: Set[T]): Set[Option[T]] = s.map[Option[T]](Some.apply)
  def addNone[T](s: Set[T]): Set[Option[T]] = wrapSome(s) + None

  def graphToMap[E, K, V](graph: Iterable[E])(f: E => (K, V)): Map[K, Set[V]] =
    graph.view
      .map(f)
      .groupBy(_._1)
      .view
      .mapValues(_.map { case (k, v) => v }.toSet)
      .toMap
      .withDefaultValue(Set.empty)

  def mapToGraph[E, K, V](map: Map[K, Set[V]])(f: ((K, V)) => E): Iterable[E] =
    for ((k, vs) <- map; v <- vs) yield f(k, v)

  def makeZ3Context() = {
    val cfg = new java.util.HashMap[String, String]()
    cfg.put("model", "true")
    new com.microsoft.z3.Context(cfg)
  }

  def withZ3Context[T](body: com.microsoft.z3.Context => T): T = {
    val ctx = makeZ3Context()
    val res = body(ctx)
    ctx.close()
    res
  }

  class Cacher[T](f: => T) {
    private var cache: Option[T] = None
    def getOrCalc(): T = cache.getOrElse {
      val t = f
      cache = Some(t)
      t
    }
    def reset(): Unit = cache = None
  }
  object Cacher {
    def apply[T](f: => T) = new Cacher(f)
  }

  /** 重複しない新しい要素を生成 */
  trait Gen[T] {
    def gen(elms: Iterable[T]): T
  }

  implicit object GenInt extends Gen[Int] {
    def gen(elms: Iterable[Int]): Int =
      if (elms.isEmpty) 0 else elms.max + 1
  }

  // case class NonRegAssertNotCmd(terms: Seq[Terms.Term]) extends Commands.CommandExtension {
  //   override def print(ctx: PrintingContext): Unit = {
  //     ctx.print(s"(nonreg/assert-not ${terms.mkString("\n", "\n", "")})")
  //   }
  //   override def transform(tt: TreeTransformer)(context: tt.C): (Commands.Command, tt.R) = {
  //     val (rts, rcs) = terms.map { tt.transform(_, context) }.unzip
  //     (NonRegAssertNotCmd(rts), tt.combine(this, context, rcs))
  //   }
  // }
}
