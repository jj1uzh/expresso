package com.github.kmn4.expresso.strategy

import com.microsoft.z3
import com.github.kmn4.expresso._
import com.github.kmn4.expresso.math._
import com.github.kmn4.expresso.machine._
import com.github.kmn4.expresso.language.Constraint._
import Transduction._
import com.typesafe.scalalogging.Logger
import com.typesafe.scalalogging.StrictLogging

/**
  * 逆像計算に基づくアルゴリズム．
  *
  * TODO
  *  - 最後の eagerIntersect をしない
  *  - CDCL 的に探索木を刈り取る ?
  *  - 逆像計算自体の戻り値を Iterator にする
  *  - IdentifiedPA を全体でユニークな ID が割り当てられるようにし，
  *    同じ PA を (同じ PST で) 逆像するときはキャッシュを使う
  */
class PreImageStrategy(implicit logger: Logger) extends Strategy {
  private type PA = IdentifiedPA[Int, Char, Int, String]
  private type PR = ParikhRelation[Int, Char, Int, String]
  private type PST = ParikhSST[Int, Either[Char, Int], Char, Int, Int, String]
  private object WithTime {
    private val time = collection.mutable.Map.empty[String, Long]
    def apply[T](name: String)(body: => T): T = {
      val start = System.nanoTime()
      val t = body
      val done = System.nanoTime()
      time(name) = time.getOrElseUpdate(name, 0) + done - start
      t
    }
    def info: Unit = time.foreach {
      case (name, time) =>
        logger.info(s"$name\t${time / 1000000}ms")
    }
  }

  // pr の satisfialibity を確認
  // sat なら x_0, x_1, ... の値と i_0, i_1, ... の値を返す
  // 等式の左辺に現れる変数の値は返さないことに注意
  private def checkClause(pr: PR): Option[(Seq[String], Map[String, Int])] = {
    // 共通部分をとる
    // 異なる文字列変数に対応する PA のログ変数がかぶらないようにする
    logger.trace("last intersect")
    val uniqPR = WithTime("intersect")(ParikhRelation.eagerIntersect(pr))
    val sizes = uniqPR.parikhAutomata.map { pas =>
      val pa = pas.head.pa
      pa.states.size
    }
//    logger.info(s"intersect done\t$sizes")
    val psts: Seq[ParikhSST[Int, Char, Char, Unit, (Int /* index */, Int /* l */ ), String]] =
      uniqPR.parikhAutomata.zipWithIndex.map {
        case (Seq(IdentifiedPA(_, pa)), idx) => pa.toIdentityParikhSST.renamed(identity _, identity _, l => (idx, l))
      }
    // 整数変数 int_*, ログ変数 log_*_*, 束縛変数 bound_*
    val formula = WithTime("formula") {
      val pstFormulas = psts.flatMap { pst =>
        val accept = pst.acceptFormula.renameVars {
          case Left(i)         => s"int_$i"
          case Right((idx, l)) => s"log_${idx}_$l"
        }
        val parikh = pst.parikhImageFormula.renameVars {
          case Left(b)         => s"bound_$b"
          case Right((idx, l)) => s"log_${idx}_$l"
        }
        Seq(accept, parikh)
      }
      val globalFormulas = pr.globalFormulas.map(_.renameVars(i => s"int_$i"))
      Presburger.Conj(pstFormulas ++ globalFormulas)
    }
    withZ3Context { ctx =>
      val solver = ctx.mkSolver()
      val z3Expr = Presburger.Formula.formulaToZ3Expr(ctx, Map.empty[String, z3.IntExpr], formula)
//      println(s"Z3 Expression: $z3Expr")
      solver.add(z3Expr)
      logger.debug("check start")
      val status = WithTime("check")(solver.check())
      logger.debug("check done")
      if (status == z3.Status.SATISFIABLE) {
        logger.debug("sat")
        val model = solver.getModel()
        def getValue(name: String): Int = model.eval(ctx.mkIntConst(name), false).toString().toInt
        // 得られたモデルの値を出力する入力文字列を探す
        val inputs = psts.map { pst =>
          val v = pst.ls.map { case il @ (idx, l) => il -> getValue(s"log_${idx}_$l") }.toMap
          pst.inputOutputFor(v)._1.mkString
        }
        // 整数変数のモデル
        val pat = "^int_(.*)".r
        val intVarValues = formula.freeVars.collect {
          case i @ pat(name) => name -> getValue(i)
        }
        Some((inputs, intVarValues.toMap))
      } else {
        logger.debug("unsat")
        None
      }
    }
  }
  private var models: Output = None
  override def checkSat(constraint: Input): Boolean = {
    java.lang.Runtime.getRuntime().addShutdownHook(new Thread(() => WithTime.info))
    constraint.assignments.foreach(a => logger.trace(a.toString))
    val config @ Configuration(trans, _) = organize(constraint)
    val relIter = iteratePreImage(config)
    models = {
      val iter = for {
        rel <- relIter
        (sm, im) <- checkClause(rel)
      } yield {
        // trans を使って左辺の文字列変数を計算
        def lhs(strModel: Seq[String], p: PST, rs: Seq[Int]): String = {
          type A = Either[Char, Int] // こういうのは型メンバーを使うと楽そう
          val words = rs.zipWithIndex.foldLeft(Seq.empty[A]) {
            case (acc, (x, i)) =>
              val w = strModel(x)
              acc ++ w.map(Left.apply) :+ Right(i)
          }
          val input = words.dropRight(1)
          val set = p.transduce(input, im)
          // NOTE 関数的であることを要求
          if (set.size != 1) throw new Exception("Not functional transduction")
          set.head.mkString
        }
        val sm2 = trans.foldLeft(sm) { case (acc, PreImagable(pst, rhs)) => acc :+ lhs(acc, pst, rhs) }
        (sm2, im)
      }
      iter.nextOption()
    }
    WithTime.info
    models.nonEmpty
  }

  override def issat: Boolean = models.nonEmpty
  override def checkSatForall(v: String, constraint: Input): Boolean = {
    println("Preimage strategy: forall is not implemented. Returns true.")
    true
  }

  override def getModel(): Output = models

  private def assingmentToPSST[S](assignment: AtomicAssignment[S], alphabet: Set[Char]): PST =
    assignment match {
      case ParikhAssignment(_, trans, _) => {
        val psst = trans.toParikhSST(alphabet)
        psst.copy(
          inSet = psst.inSet.map(Left.apply),
          edges = psst.edges.map(e => e.copy(_2 = Left(e._2)))
        )
      }
      case CatAssignment(_, wordAndVars) => {
        // TODO wordsAndVars が少なくとも1つの文字列変数を含むことを仮定している点を緩和
        type Q = Int
        type A = Either[Char, Int]
        type B = Char
        type X = Unit
        type E = NSST.Edge[Q, A, B, X]
        type O = NSST.Out[Q, X, B]
        val depSize: Int = assignment.dependeeVars.size
        val states: Set[Q] = (0 until depSize).toSet
        val inSet: Set[A] = alphabet.map(Left.apply) ++ (0 to depSize - 2).map(Right.apply).toSet
        // w0 x0 w1 ... wn-1 xn-1 wn のときの w0 ... wn
        val words: Seq[Seq[Char]] = {
          wordAndVars.foldRight(List(Seq.empty[B])) {
            case (Left(s), h :: rst) => (s ++ h) :: rst
            case (Right(_), acc)     => Nil :: acc
            case _                   => throw new Exception("This cannot be the case")
          }
        }
        val edges: Set[E] = {
          val loop: Iterable[E] =
            for {
              i <- 0 to depSize - 1
              a <- alphabet
            } yield {
              val adding = List(Cop1(()), Cop2(a))
              val m = Map(() -> adding)
              (i, Left(a), m, i)
            }
          val next: Iterable[E] =
            (0 to depSize - 2).map { i =>
              val xbs = Cop1(()) :: words(i + 1).map(Cop2.apply).toList
              val m = Map(() -> xbs)
              (i, Right(i), m, i + 1)
            }
          (loop ++ next).toSet
        }
        val outGraph: Set[O] = {
          val added: Cupstar[X, B] =
            (words(0).map(Cop2.apply) ++ Seq(Cop1(())) ++ words.last.map(Cop2.apply)).toList
          Set((depSize - 1, added))
        }
        NSST(
          states,
          inSet,
          Set(()),
          edges,
          0,
          graphToMap(outGraph)(identity)
        ).renamed.toParikhSST
      }
      case InsertAssignment(lhs, ins, div, idx) => {
        import Presburger._
        import Presburger.Sugar._
        val ls @ Seq(index, length) = Seq(0, 1)
        val states @ Seq(collect, seek, append) = Seq(0, 1, 2)
        val xs @ Seq(x, y) = Seq(0, 1)
        type M = Map[Int, List[Cop[Int, Char]]]
        val unit: M = Map(x -> List(Cop1(x)), y -> List(Cop1(y)))
        val appendX = alphabet.map(a => a -> (unit + (x -> List(Cop1(x), Cop2(a))))).toMap
        val appendY = alphabet.map(a => a -> (unit + (y -> List(Cop1(y), Cop2(a))))).toMap
        val (zz, oo, zo) = (
          Map(index -> 0, length -> 0),
          Map(index -> 1, length -> 1),
          Map(index -> 0, length -> 1)
        )
        type Q = Int
        type A = Either[Char, Int]
        type V = Map[Int, Int]
        type E = (Q, A, M, V, Q)
        val edges: Iterable[E] = {
          val collecting: Iterable[E] = alphabet.map(a => (collect, Left(a), appendX(a), zz, collect))
          val toSeek: E = (collect, Right(0), unit, zz, seek)
          val seeking: Iterable[E] = alphabet.map(a => (seek, Left(a), appendY(a), oo, seek))
          val toAppend: Iterable[E] = alphabet.map { a =>
            val yxa = List(Cop1(y), Cop1(x), Cop2(a))
            val m = Map(x -> Nil, y -> yxa)
            (seek, Left(a), m, zo, append)
          }
          val appending: Iterable[E] = alphabet.map(a => (append, Left(a), appendY(a), zo, append))
          Iterable(toSeek) ++ (collecting ++ seeking ++ toAppend ++ appending)
        }
        val outGraph = Set(
          (seek, List(Cop1(y), Cop1(x)), zz),
          (append, List(Cop1(y)), zz)
        )
        type IVar = Either[String, Int]
        val Seq(idxVar, indexVar, lengthVar): Seq[Var[IVar]] =
          Seq(Var(Left(idx)), Var(Right(index)), Var(Right(length)))
        val formulas = Seq[Presburger.Formula[IVar]](
          idxVar < 0 ==> (indexVar === 0),
          (const(0) <= idxVar && idxVar <= lengthVar) ==> (indexVar === idxVar),
          idxVar >= lengthVar ==> (indexVar === lengthVar)
        )
        ParikhSST(
          states.toSet,
          alphabet.map[Either[Char, Int]](Left.apply) + Right(0),
          xs.toSet,
          ls.toSet,
          Set(idx),
          edges.toSet,
          collect,
          outGraph.toSet,
          formulas
        )
      }
    }

  private implicit class AssignmentToPSST[S](assignment: AtomicAssignment[S]) {
    def toExpPST(alphabet: Set[Char]): PST = assingmentToPSST(assignment, alphabet)
  }

  private case class PreImagable(pst: PST, rhs: Seq[Int]) {
    // next() すると op^-1(lang) から1つずつ選んだ組を返す.
    // PR において各変数の言語が単一の PA で表されれているので不要だが，
    // そうでない場合も考えるかもしれないので残しておく．
    def preImages(langs: Seq[PA], maxID: Int): Iterator[PR] = {
      val idInc = rhs.length // maxID は逆像をとるたび idInc 分だけ増える
      var mxid = maxID - idInc
      val preImages: Seq[PreImage[Int, Char, Int, String]] = langs.map { lang =>
        mxid += idInc
        WithTime("preimage")(pst.preImage(lang, mxid))
      }
      val choices: Iterator[Seq[PR]] = choose(preImages)
      val emptyRel: PR = ParikhRelation(Seq.empty, Seq.empty)
      choices.map(seq => seq.fold(emptyRel)(_ lazyIntersect _))
    }

    // rel の最後の成分の逆像をとって，それより前の成分と組み合わせたものを返す:
    // (x1, x2, .., xn) \in rel and xn = pst(rhs)
    // <==> (x1, x2, .., x{n-1}) \in res
    def preImages(rel: PR): Iterator[PR] = {
      val lhs = rel.parikhAutomata.length - 1
      logger.trace(s"LHS : ${lhs}")
      val automata = rel.parikhAutomata(lhs)
      val choices = preImages(automata, rel.maxID)
      val res = choices.map { newRel => ParikhRelation.impose(rel, newRel, rhs) }
      res.map { rel => rel.copy(parikhAutomata = rel.parikhAutomata.dropRight(1)) }
    }

    private def choose[A](ll: Seq[Seq[A]]): Seq[Seq[A]] =
      if (ll.isEmpty) Seq(Seq.empty)
      else
        for {
          l <- ll
          a <- l
          rec <- choose(ll.tail)
        } yield a +: rec

    private def choose[A](si: Seq[Iterator[A]]): Iterator[Seq[A]] =
      if (si.isEmpty) Iterator.empty
      else if (si.size == 1) si.head.map(a => Seq(a))
      else choose(si.map(iter => LazyList.from(iter))).iterator

  }

  // 条件: transductions は lhs について昇順
  // 条件: relation の PA は lhs について昇順で，0 から順にならぶ
  // 条件: transductions の最大 lhs == relation の最大 lhs
  private case class Configuration(
      transductions: Seq[PreImagable],
      relation: ParikhRelation[Int, Char, Int, String]
  )

  // トランスダクション，言語，整数制約を PST と PR にまとめる
  // NOTE assignments は左辺変数でソート済みと仮定
  private def organize(constraint: Input): Configuration = {
    // val Triple(assignments, assertions, _) = triple
    val maxVar = constraint.stringVarNumber - 1
    val alphabet = constraint.alphabet
    // assignments から PST を構成
    val transes = constraint.assignments.map { a => PreImagable(a.toExpPST(alphabet), a.dependeeVars) }
    // assertions と arithFormulas から PR が得られる
    logger.info("start to construct PR")
    val rel = {
      val langMap = constraint.assertions.groupMap(_.stringVar)(_.lang)
      val paMap = langMap.view.mapValues { langs =>
        val (dfas, pas) = langs partitionMap {
          case language.ParikhLanguage.FromRegExp(re) =>
            Left { re.toNFA(alphabet).toDFA.minimized }
          case lang => Right(lang.toParikhAutomaton(alphabet))
        }
        val dfa: Option[DFA[Int, Char]] = dfas.reduceOption[DFA[Int, Char]] {
          case (acc, d) => acc.intersect(d).minimized
        }
        pas ++ dfa.map(_.toParikhAutomaton[Int, String])
      }
      val pas = (0 to maxVar).map { idx =>
        val all = ParikhAutomaton.universal[Int, Char, Int, String](0, alphabet)
        paMap.getOrElse(idx, Seq.empty).foldLeft(all) {
          case (acc, pa) => acc.intersect(pa).renamed
        }
      }
      val withID = pas.zipWithIndex.map { case (pa, idx) => Seq(IdentifiedPA(idx, pa)) }
      ParikhRelation(withID, constraint.arithFormulas)
    }
    logger.info("PR construction done")
    Configuration(transes, rel)
  }

  // transductions が非空である間 relation の逆像を計算する
  // disjunction が現れると非決定的な選択をする．この非決定性は Iterator が表している．
  private def iteratePreImage(config: Configuration): Iterator[ParikhRelation[Int, Char, Int, String]] = {
    val Configuration(operations, relation) = config
    operations.foldRight(Iterator(relation)) {
      case (op, acc) =>
        acc.flatMap { rel =>
          val uniqLhsPR = ParikhRelation.eagerIntersect(rel, rel.parikhAutomata.length - 1)
          op.preImages(uniqLhsPR)
        }
    }
  }

}

object PreImageStrategy extends StrategyFactory[PreImageStrategy] with StrictLogging {
  def mkStrategy: PreImageStrategy = new PreImageStrategy()(logger)
}
