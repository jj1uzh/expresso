package com.github.kmn4.sst

import com.github.kmn4.sst.Presburger

package object experimental {
  type Formulas[I, L] = Seq[Presburger.Formula[Either[I, L]]]
  type GlobalFormulas[I] = Seq[Presburger.Formula[I]]
  // PreImage は実際のところ LazyList
  type PreImage[Q, A, L, I] = Seq[(Seq[ParikhAutomaton[Q, A, L, I]], GlobalFormulas[I])]
  type PST = ParikhSST[Int, Either[Char, Int], Char, Int, Int, String]
}
