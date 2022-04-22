package com.github.kmn4.expresso.language

import com.github.kmn4.expresso._
import com.github.kmn4.expresso.machine._
import com.github.kmn4.expresso.math._
import com.github.kmn4.expresso.math.Presburger.Sugar._

sealed trait ParikhLanguage[C, I] {
  def toParikhAutomaton(alphabet: Set[C]): ParikhAutomaton[Int, C, Int, I]
  def usedAlphabet: Set[C]
}

object ParikhLanguage {

  case class Disjunction[C, I](lang1: ParikhLanguage[C, I], lang2: ParikhLanguage[C, I]) extends ParikhLanguage[C, I] {
    override def usedAlphabet: Set[C] = lang1.usedAlphabet ++ lang2.usedAlphabet
    override def toParikhAutomaton(alphabet: Set[C]): ParikhAutomaton[Int,C,Int,I] =
      lang1.toParikhAutomaton(alphabet)
  }

  case class FromRegExp[C, I](val re: RegExp[C]) extends ParikhLanguage[C, I] {

    def usedAlphabet: Set[C] = re.usedAlphabet

    def toParikhAutomaton(alphabet: Set[C]): ParikhAutomaton[Int, C, Int, I] =
      re.toNFA(alphabet).toDFA.toParikhAutomaton.renamed
  }

  case class Length[A, I](lenVar: I) extends ParikhLanguage[A, I] {
    def usedAlphabet: Set[A] = Set.empty

    def toParikhAutomaton(alphabet: Set[A]): ParikhAutomaton[Int, A, Int, I] =
      ParikhAutomaton(
        Set(0),
        alphabet,
        Set(0),
        Set(lenVar),
        alphabet.map(a => (0, a, Map(0 -> 1), 0)),
        0,
        Set((0, Map(0 -> 0))),
        Seq(Presburger.Eq(Presburger.Var(Left(lenVar)), Presburger.Var(Right(0))))
      )
  }

  case class CountChar[A, I](charNum: I, targetChar: A) extends ParikhLanguage[A, I] {
    def usedAlphabet: Set[A] = Set(targetChar)
    def toParikhAutomaton(alphabet: Set[A]): ParikhAutomaton[Int, A, Int, I] =
      ParikhAutomaton(
        states = Set(0),
        inSet = alphabet,
        ls = Set(0),
        is = Set(charNum),
        edges = alphabet
          .filterNot(_ == targetChar)
          .map(a => (0, a, Map(0 -> 0), 0)) + ((0, targetChar, Map(0 -> 1), 0)),
        q0 = 0,
        acceptRelation = Set((0, Map(0 -> 0))),
        acceptFormulas = Seq(Presburger.Eq(Presburger.Var(Left(charNum)), Presburger.Var(Right(0))))
      )
  }

  // (= j (str.indexof x w c)) --> IndexOfConst(w, c, j)
  case class IndexOfConst[A, I](target: Seq[A], from: Int, jName: I) extends ParikhLanguage[A, I] {
    def usedAlphabet: Set[A] = target.toSet

    def toParikhAutomaton(alphabet: Set[A]): ParikhAutomaton[Int, A, Int, I] = {
      import Presburger._
      type L = Int
      type T = Term[Either[I, L]]
      val j: T = Var(Left(jName))
      val input: T = Var(Right(0))
      val untilMatch: T = Var(Right(1))
      type Q = Int
      type Edges = Iterable[(Q, A, Map[L, Int], Q)]
      val dfa = postfixDFA(target, alphabet)
      val skipStates = (-from to -1).toSet
      val skipEdges = for {
        q <- skipStates
        a <- alphabet
      } yield (q, a, Map(0 -> 1, 1 -> 1), q + 1)
      val skipOutGraph = skipStates.map(q => (q, Map(0 -> 0, 1 -> 0)))
      val states = dfa.states
      val edges: Edges = {
        for {
          q <- states
          a <- alphabet
        } yield {
          val r = dfa.transition.getOrElse((q, a), q)
          val skipped =
            if (dfa.finalStates(r)) 0
            else q + 1 - r
          val v = Map(0 -> 1, 1 -> skipped)
          (q, a, v, r)
        }
      }
      val outGraph =
        // On each state q, DFA has partially matched prefix of target string.
        states.map(q => (q, Map(0 -> 0, 1 -> (q % target.length))))
      val acceptFormulas = Seq(
        (input < from || input <= untilMatch) ==> (j === -1),
        (input >= from && input > untilMatch) ==> (j === untilMatch)
      )
      ParikhAutomaton[Q, A, L, I](
        states ++ skipStates,
        alphabet,
        Set(0, 1),
        Set(jName),
        edges.toSet ++ skipEdges,
        -from,
        outGraph ++ skipOutGraph,
        acceptFormulas
      )
    }
  }

  // (= j (str.indexof x w i)) --> x ∈ IndexOf(w, i, j)
  case class IndexOf[A, I](target: Seq[A], iName: I, jName: I) extends ParikhLanguage[A, I] {
    def usedAlphabet: Set[A] = target.toSet

    def toParikhAutomaton(alphabet: Set[A]): ParikhAutomaton[Int, A, Int, I] = {
      import Presburger._
      type L = Int
      type T = Term[Either[I, L]]
      val i: T = Var(Left(iName))
      val j: T = Var(Left(jName))
      val input: T = Var(Right(0))
      val untilMatch: T = Var(Right(1))
      val skipped: T = Var(Right(2))
      type Q = Int
      type Edges = Iterable[(Q, A, Map[L, Int], Q)]
      val dfa = postfixDFA(target, alphabet)
      val states = dfa.states
      val edges: Edges = {
        for {
          q <- states
          a <- alphabet
        } yield {
          val r = dfa.transition.getOrElse((q, a), q)
          val skipped =
            if (dfa.finalStates(r)) 0
            else q + 1 - r
          val v = Map(0 -> 1, 1 -> skipped, 2 -> 0)
          (q, a, v, r)
        }
      }
      val outGraph =
        // On each state q, DFA has partially matched prefix of target string.
        states.map(q => (q, Map(0 -> 0, 1 -> (q % target.length), 2 -> 0)))
      val acceptFormulas = Seq(
        (i < 0 || input <= i) ==> (j === -1),
        (i >= 0 && i < input) ==> (skipped === i),
        (input <= untilMatch) ==> (j === -1),
        (input > untilMatch) ==> (j === untilMatch)
      )
      val skipState = -1
      val fromSkipState = {
        val trans = graphToMap(edges) { case (q, a, v, r) => (q, a) -> (r, v) }
        alphabet.flatMap { a =>
          trans(dfa.q0, a).map { case (r, v) =>
            (skipState, a, v + (2 -> 0), r)
          } +
            ((skipState, a, Map(0 -> 1, 1 -> 1, 2 -> 1), skipState))
        }
      }
      ParikhAutomaton[Q, A, L, I](
        states + skipState,
        alphabet,
        Set(0, 1, 2),
        Set(iName, jName),
        edges.toSet ++ fromSkipState,
        skipState,
        outGraph + ((skipState, Map(0 -> 0, 1 -> 0, 2 -> 0))),
        acceptFormulas
      )
    }
  }
  case class CodeAt[I](iName: I, cName: I) extends ParikhLanguage[Char, I] {
    def usedAlphabet: Set[Char] = Set.empty
    def toParikhAutomaton(alphabet: Set[Char]): ParikhAutomaton[Int, Char, Int, I] = {
      import Presburger._
      val (q0, qf) = (0, 1)
      type T = Term[Either[I, Int]]
      val (i, c): (T, T) = (Var(Left(iName)), Var(Left(cName)))
      val (input, index, code): (T, T, T) = (Var(Right(0)), Var(Right(1)), Var(Right(2)))
      ParikhAutomaton(
        Set(q0, qf),
        alphabet,
        Set(0, 1, 2),
        Set(iName, cName),
        alphabet.flatMap { a =>
          Iterable(
            (q0, a, Map(0 -> 1, 1 -> 1, 2 -> 0), q0),
            (q0, a, Map(0 -> 1, 1 -> 0, 2 -> a.toInt), qf),
            (qf, a, Map(0 -> 1, 1 -> 0, 2 -> 0), qf)
          )
        },
        q0,
        Set((q0, Map(0 -> 0, 1 -> 0, 2 -> -1)), (qf, Map(0 -> 0, 1 -> 0, 2 -> 0))),
        Seq(
          (i >= 0 && i < input) ==> (i === index && c === code),
          (i < 0 || input <= i) ==> (c === -1)
        )
      )
    }
  }
}
