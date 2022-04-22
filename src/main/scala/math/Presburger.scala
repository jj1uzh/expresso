package com.github.kmn4.expresso.math

import com.microsoft.z3

object Presburger {

  /** Types and constructers for Presburger formula */
  sealed trait Term[X] {
    def eval(valuation: Map[X, Int]): Int = this match {
      case Const(i)    => i
      case Var(x)      => valuation(x)
      case Add(ts)     => ts.map(_.eval(valuation)).sum
      case Sub(t1, t2) => t1.eval(valuation) - t2.eval(valuation)
      case Mult(i, t)  => i.eval(valuation) * t.eval(valuation)
    }

    def freeVars: Set[X] = this match {
      case Var(x) => Set(x)
      case _      => Set.empty
    }

    def size: Int = this match {
      case Const(i)    => 0
      case Var(x)      => 0
      case Add(ts)     => ts.map(_.size).sum + 1
      case Sub(t1, t2) => t1.size + t2.size + 1
      case Mult(c, t)  => c.size + t.size + 1
    }
  }
  case class Const[X](i: Int) extends Term[X]
  case class Var[X](x: X) extends Term[X]
  case class Add[X](ts: Seq[Term[X]]) extends Term[X]
  case class Sub[X](t1: Term[X], t2: Term[X]) extends Term[X]
  // Mult は当初定数倍を意図していたが，その後の変更で任意の掛け算として使われるようになった．
  case class Mult[X](c: Term[X], t: Term[X]) extends Term[X]
  sealed trait Formula[X] {
    def smtlib: String = Formula.formulaToSMTLIB(this)

    def renameVars[Y](renamer: X => Y): Formula[Y] = Formula.renameVars(this)(renamer)

    def freeVars: Set[X] = this match {
      case Top() | Bot() => Set.empty
      case Eq(t1, t2)    => t1.freeVars ++ t2.freeVars
      case Lt(t1, t2)    => t1.freeVars ++ t2.freeVars
      case Le(t1, t2)    => t1.freeVars ++ t2.freeVars
      case Conj(fs)      => fs.flatMap(_.freeVars).toSet
      case Disj(fs)      => fs.flatMap(_.freeVars).toSet
      case Not(f)        => f.freeVars
      case Exists(vs, f) => f.freeVars -- vs.map(_.x)
    }

    /** @throws java.lang.UnsupportedOperationException if this contains Exists. */
    def eval(valuation: Map[X, Int]): Boolean = this match {
      case Top()        => true
      case Bot()        => false
      case Eq(t1, t2)   => t1.eval(valuation) == t2.eval(valuation)
      case Lt(t1, t2)   => t1.eval(valuation) < t2.eval(valuation)
      case Le(t1, t2)   => t1.eval(valuation) <= t2.eval(valuation)
      case Conj(fs)     => fs.find(!_.eval(valuation)).isEmpty
      case Disj(fs)     => fs.find(_.eval(valuation)).nonEmpty
      case Not(f)       => !f.eval(valuation)
      case Exists(_, _) => throw new UnsupportedOperationException("Cannot evaluate formula with quantifier.")
    }

    def size: Int = this match {
      case Top()         => 1
      case Bot()         => 1
      case Eq(t1, t2)    => t1.size + t2.size + 1
      case Lt(t1, t2)    => t1.size + t2.size + 1
      case Le(t1, t2)    => t1.size + t2.size + 1
      case Conj(fs)      => fs.map(_.size).sum + 1
      case Disj(fs)      => fs.map(_.size).sum + 1
      case Not(f)        => f.size + 1
      case Exists(vs, f) => vs.map(_.size).sum + f.size + 1
    }
  }
  case class Top[X]() extends Formula[X]
  case class Bot[X]() extends Formula[X]
  case class Eq[X](t1: Term[X], t2: Term[X]) extends Formula[X]
  case class Lt[X](t1: Term[X], t2: Term[X]) extends Formula[X]
  case class Le[X](t1: Term[X], t2: Term[X]) extends Formula[X]
  def Gt[X](t1: Term[X], t2: Term[X]): Formula[X] = Lt(t2, t1)
  def Ge[X](t1: Term[X], t2: Term[X]): Formula[X] = Le(t2, t1)
  case class Conj[X](fs: Seq[Formula[X]]) extends Formula[X]
  case class Disj[X](fs: Seq[Formula[X]]) extends Formula[X]
  case class Not[X](f: Formula[X]) extends Formula[X]
  def Implies[X](pre: Formula[X], post: Formula[X]): Formula[X] = Disj(Seq(Not(pre), post))
  def Equiv[X](f1: Formula[X], f2: Formula[X]): Formula[X] = Conj(Seq(Implies(f1, f2), Implies(f2, f1)))
  case class Exists[X](vs: Seq[Var[X]], f: Formula[X]) extends Formula[X]

  object Term {
    def termToSMTLIB[X](t: Term[X]): String = t match {
      case Const(i)     => i.toString()
      case Var(x)       => x.toString()
      case Add(ts)      => s"""(+ 0 ${ts.map(termToSMTLIB).mkString(" ")})"""
      case Sub(t1, t2)  => s"(- ${termToSMTLIB(t1)} ${termToSMTLIB(t2)})"
      case Mult(t1, t2) => s"(* ${termToSMTLIB(t1)} ${termToSMTLIB(t2)})"
    }
  }

  object Formula {
    // 代入前後の変数の型は同じでなければならない．
    // ∃x φ(x) が与えられた時 φ(x) について再帰することを考える．
    // φ だけみると x が束縛変数かどうかはわからない．
    // そのため x => if (bounded(x)) Var(x) else subst(x) を新しい subst にして再帰する．
    // 代入前後の型が異なると, then 節をどうすればよいか困る．
    def substitute[X](f: Formula[X])(subst: X => Term[X]): Formula[X] =
      substituteBound(f)(x => subst(x))(identity(_))

    // 束縛変数は substBound で代入する
    // 定義されていなかったら実行時エラーになる
    def substituteBound[X, Y](
        f: Formula[X]
    )(subst: PartialFunction[X, Term[Y]])(substBound: PartialFunction[X, Y]): Formula[Y] = {
      def tm(t: Term[X]): Term[Y] = {
        def aux(t: Term[X]): Term[Y] = t match {
          case Const(i)          => Const(i)
          case Var(x)            => subst(x)
          case Add(ts)           => Add(ts.map(aux))
          case Sub(t1, t2)       => Sub(aux(t1), aux(t2))
          case Mult(Const(i), t) => Mult(Const(i), aux(t))
          case Mult(i, t)        => Mult(aux(i), aux(t))
        }
        aux(t)
      }
      def aux(f: Formula[X]): Formula[Y] = f match {
        case Top()      => Top()
        case Bot()      => Bot()
        case Eq(t1, t2) => Eq(tm(t1), tm(t2))
        case Lt(t1, t2) => Lt(tm(t1), tm(t2))
        case Le(t1, t2) => Le(tm(t1), tm(t2))
        case Conj(fs)   => Conj(fs.map(aux))
        case Disj(fs)   => Disj(fs.map(aux))
        case Not(f)     => Not(aux(f))
        case Exists(xs, f) =>
          val ys = xs.map { case Var(x) => Var(substBound(x)) }
          val bounded = xs.map { case Var(x) => x }.toSet
          val newSubst: PartialFunction[X, Term[Y]] = {
            case x if bounded(x) => Var(substBound(x))
            case x               => subst(x)
          }
          Exists(ys, substituteBound(f)(newSubst)(substBound))
      }
      aux(f)
    }

    // B: 束縛出現するかもしれない型
    // F: 束縛出現しないことが保証されている型
    // N: F の変換後
    def substituteFreeVars[F, B, N](
        f: Formula[Either[B, F]]
    )(subst: F => Term[Either[B, N]]): Formula[Either[B, N]] = {
      substituteBound(f) {
        case Left(b)     => Var(Left(b): Either[B, N])
        case Right(free) => subst(free)
      } { case Left(b) => Left(b): Either[B, N] }

    }
    // NOTE renamer should be injective
    def renameVars[X, Y](f: Formula[X])(renamer: X => Y): Formula[Y] = {
      def tm(t: Term[X]): Term[Y] = {
        def aux(t: Term[X]): Term[Y] = t match {
          case Const(i)          => Const(i)
          case Var(x)            => Var(renamer(x))
          case Add(ts)           => Add(ts.map(aux))
          case Sub(t1, t2)       => Sub(aux(t1), aux(t2))
          case Mult(Const(i), t) => Mult(Const(i), aux(t))
          case Mult(i, t)        => Mult(aux(i), aux(t))
        }
        aux(t)
      }
      def aux(f: Formula[X]): Formula[Y] = f match {
        case Top()         => Top()
        case Bot()         => Bot()
        case Eq(t1, t2)    => Eq(tm(t1), tm(t2))
        case Lt(t1, t2)    => Lt(tm(t1), tm(t2))
        case Le(t1, t2)    => Le(tm(t1), tm(t2))
        case Conj(fs)      => Conj(fs.map(aux))
        case Disj(fs)      => Disj(fs.map(aux))
        case Not(f)        => Not(aux(f))
        case Exists(xs, f) => Exists(xs.map { case Var(x) => Var(renamer(x)) }, aux(f))
      }
      aux(f)
    }
    def formulaToSMTLIB[X](f: Formula[X]): String = f match {
      case Top()      => "(= 0 0)"
      case Bot()      => "(= 0 1)"
      case Eq(t1, t2) => s"(= ${Term.termToSMTLIB(t1)} ${Term.termToSMTLIB(t2)})"
      case Lt(t1, t2) => s"(< ${Term.termToSMTLIB(t1)} ${Term.termToSMTLIB(t2)})"
      case Le(t1, t2) => s"(<= ${Term.termToSMTLIB(t1)} ${Term.termToSMTLIB(t2)})"
      case Conj(fs) => {
        val fsString = fs.map(formulaToSMTLIB).mkString(" ")
        s"(and true $fsString)"
      }
      case Disj(fs) => {
        val fsString = fs.map(formulaToSMTLIB).mkString(" ")
        s"(or false $fsString)"
      }
      case Not(f) => s"(not ${formulaToSMTLIB(f)})"
      case Exists(xs, f) => {
        val xsString = xs.map { case Var(x) => s"(${x.toString()} Int)" }.mkString(" ")
        s"(exists (${xsString}) ${formulaToSMTLIB(f)})"
      }
    }

    /** Convert a given formula to z3.BoolExpr. */
    def formulaToZ3Expr[X](
        ctx: z3.Context,
        freeVars: Map[X, z3.IntExpr],
        f: Formula[X]
    ): z3.BoolExpr = {
      var varMap = freeVars
      val trueExpr = ctx.mkBool(true)
      val falseExpr = ctx.mkBool(false)
      def newVar(x: X): z3.IntExpr = {
        val e = ctx.mkIntConst(x.toString())
        varMap += (x -> e)
        e
      }
      def fromTerm(t: Term[X]): z3.ArithExpr = t match {
        case Const(i)    => ctx.mkInt(i)
        case Var(x)      => varMap.getOrElse(x, newVar(x))
        case Add(ts)     => ctx.mkAdd(ts.map(fromTerm): _*)
        case Sub(t1, t2) => ctx.mkSub(fromTerm(t1), fromTerm(t2))
        case Mult(c, t)  => ctx.mkMul(fromTerm(c), fromTerm(t))
      }
      def fromFormula(f: Formula[X]): z3.BoolExpr = f match {
        case Top()      => trueExpr
        case Bot()      => falseExpr
        case Eq(t1, t2) => ctx.mkEq(fromTerm(t1), fromTerm(t2))
        case Lt(t1, t2) => ctx.mkLt(fromTerm(t1), fromTerm(t2))
        case Le(t1, t2) => ctx.mkLe(fromTerm(t1), fromTerm(t2))
        case Conj(fs)   => ctx.mkAnd(fs.map(fromFormula): _*)
        case Disj(fs)   => ctx.mkOr(fs.map(fromFormula): _*)
        case Not(f)     => ctx.mkNot(fromFormula(f))
        case Exists(vs, f) => {
          val xs = vs.map { case Var(x) => newVar(x) }
          val body = formulaToZ3Expr(ctx, varMap, f)
          ctx.mkExists(xs.toArray, body, 0, null, null, null, null)
        }
      }
      fromFormula(f)
    }

    def formulaToZ3ForallExpr[X](
        ctx: z3.Context,
        freeVars: Map[X, z3.IntExpr],
        quantifiedVs: Seq[Var[X]],
        f: Formula[X]
    ): z3.BoolExpr = {
      val quantifiedVars = quantifiedVs.map { case Var(x) => x -> ctx.mkIntConst(x.toString) }
      val body = formulaToZ3Expr(ctx, freeVars ++ quantifiedVars.toMap, f)
      ctx.mkForall(quantifiedVars.map(_._2).toArray, body, 0, null, null, null, null)
    }
  }

  object Sugar {
    implicit def const[X](i: Int): Term[X] = Const(i)
    implicit class TermOps[X](t: Term[X]) {
      def +(s: Term[X]): Term[X] = Add(Seq(t, s))
      def -(s: Term[X]): Term[X] = Sub(t, s)
      def *(i: Int): Term[X] = Mult(Const(i), t)
      def ===(s: Term[X]): Formula[X] = Eq(t, s)
      def <(s: Term[X]): Formula[X] = Lt(t, s)
      def <=(s: Term[X]): Formula[X] = Le(t, s)
      def >(s: Term[X]): Formula[X] = Gt(t, s)
      def >=(s: Term[X]): Formula[X] = Ge(t, s)
    }
    implicit class FormulaOps[X](f: Formula[X]) {
      def unary_! : Formula[X] = Not(f)
      def &&(g: Formula[X]): Formula[X] = Conj(Seq(f, g))
      def ||(g: Formula[X]): Formula[X] = Disj(Seq(f, g))
      def ==>(g: Formula[X]): Formula[X] = Implies(f, g)
    }
  }
}
