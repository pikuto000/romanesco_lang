// ==========================================
// Unifier.scala
// 単一化アルゴリズム（Appラムダ対応）
// ==========================================

package romanesco.Solver.core
import romanesco.Utils.Debug.logger
import LogicSymbols._

object Unifier:
  type Subst = Map[MetaId, Expr]
  def emptySubst: Subst = Map.empty

  def applySubst(e: Expr, s: Subst): Expr =
    if (s.isEmpty) e
    else e match
      case Expr.Meta(id) if s.contains(id) =>
        applySubst(s(id), s)
      case Expr.App(f, args) =>
        val nextF = applySubst(f, s)
        val nextArgs = args.map(applySubst(_, s))
        // 実際に変化があった場合のみ正規化（またはアプリケーションの場合は常に一回正規化）
        Rewriter.normalize(Expr.App(nextF, nextArgs))
      case _ => e

  def applySubstToRule(rule: CatRule, s: Subst): CatRule =
    logger.log(s"applySubstToRule: $rule, $s")
    CatRule(
      rule.name,
      applySubst(rule.lhs, s),
      applySubst(rule.rhs, s),
      rule.universals.map(applySubst(_, s))
    )

  def unify(e1: Expr, e2: Expr, subst: Subst): LazyList[Subst] =
    val r1 = applySubst(e1, subst)
    val r2 = applySubst(e2, subst)

    if r1 == r2 then
      logger.log(s"Unify Success: $r1 = $r2")
      LazyList(subst)
    else
      logger.log(s"Unifying: $r1 with $r2")
      val res = (r1, r2) match
        // --- ラムダ抽象の単一化 (App構造) ---
        case (Expr.Lam(v1, b1), Expr.Lam(v2, b2)) =>
          val newB2 = Prover.substVar(b2, v2, Expr.Var(v1))
          unify(b1, newB2, subst)

        // --- メタ変数同士のアプリケーション ---
        case (Expr.App(Expr.Meta(id1), args1), Expr.App(Expr.Meta(id2), args2))
            if id1 == id2 && args1.length == args2.length =>
          args1.zip(args2).foldLeft(LazyList(subst)) { case (sList, (a1, a2)) =>
            sList.flatMap(unify(a1, a2, _))
          }

        // --- Strict Universe Level Unification (Russell Paradox Avoidance) ---
        case (
              Expr.App(Expr.Sym(LogicSymbols.Type), args1),
              Expr.App(Expr.Sym(LogicSymbols.Type), args2)
            ) =>
          // 型宇宙のレベル構造が異なる場合は単一化しない（パラドックス回避）
          // Type(L1) = Type(L2) only if L1 == L2 structurally (same level depth)
          def getLevelDepth(e: Expr): Int = e match {
            case Expr.Meta(id) => id.ids.length
            case _             => -1 // Unknown/Var
          }
          val l1 = args1.headOption.map(getLevelDepth).getOrElse(-1)
          val l2 = args2.headOption.map(getLevelDepth).getOrElse(-1)

          if (l1 != -1 && l2 != -1 && l1 != l2) LazyList.empty
          else {
            // Standard decomposition if levels match or are variables
            unify(
              Expr.Sym(LogicSymbols.Type),
              Expr.Sym(LogicSymbols.Type),
              subst
            ).flatMap { s =>
              args1.zip(args2).foldLeft(LazyList(s)) {
                case (sList, (arg1, arg2)) =>
                  sList.flatMap(unify(arg1, arg2, _))
              }
            }
          }

        // --- 高階パターン単一化 (一般化) ---
        case (Expr.App(Expr.Meta(id), args), t) =>
          if (t == Expr.App(Expr.Meta(id), args)) LazyList(subst)
          else if (!occursCheckHigher(id, t, args))
            solveHigherOrder(id, args, t, subst)
          else LazyList.empty

        case (t, Expr.App(Expr.Meta(id), args)) =>
          if (t == Expr.App(Expr.Meta(id), args)) LazyList(subst)
          else if (!occursCheckHigher(id, t, args))
            solveHigherOrder(id, args, t, subst)
          else LazyList.empty

        // --- 基本的なメタ変数の単一化 ---
        case (Expr.Meta(id), t) if !occursCheckHigher(id, t, Nil) =>
          LazyList(subst + (id -> t))
        case (t, Expr.Meta(id)) if !occursCheckHigher(id, t, Nil) =>
          LazyList(subst + (id -> t))

        // --- 変数の単一化 ---
        case (Expr.Var(n1), Expr.Var(n2)) if n1 == n2 => LazyList(subst)

        // --- アプリケーションの単一化 (分解) ---
        case (Expr.App(h1, a1), Expr.App(h2, a2)) if a1.length == a2.length =>
          unify(h1, h2, subst).flatMap { s =>
            a1.zip(a2).foldLeft(LazyList(s)) { case (sList, (arg1, arg2)) =>
              sList.flatMap(unify(arg1, arg2, _))
            }
          }

        case _ => LazyList.empty

      // if (res.nonEmpty) logger.log(s"Unify Success: $r1 = $r2 with ${res.head}")
      res

  // 項tの中にあるtermToReplaceをreplacementに置き換える
  private def substTerm(
      t: Expr,
      termToReplace: Expr,
      replacement: Expr
  ): Expr = {
    if (t == termToReplace) replacement
    else
      t match
        case Expr.App(f, args) =>
          Expr.App(
            substTerm(f, termToReplace, replacement),
            args.map(substTerm(_, termToReplace, replacement))
          )
        case Expr.Lam(v, b) =>
          // termToReplaceの中にvが含まれている場合は置換しない（キャプチャ回避）？
          // 今回の用途ではargsは現在のスコープの項なので、ラムダ内部の変数は別物とみなす
          // ただし構造的一致を見るので、偶発的に同じ変数名だとマッチしてしまうリスクはある
          // ここでは単純な構造置換を行う
          Expr.App(
            Expr.Sym("λ"),
            List(Expr.Var(v), substTerm(b, termToReplace, replacement))
          )
        case _ => t
  }

  private def solveHigherOrder(
      id: MetaId,
      args: List[Expr],
      target: Expr,
      subst: Subst
  ): LazyList[Subst] = {
    // 引数を抽象化するための変数名を生成
    val vars = args.indices.map(i => s"v_$i").toList

    // target内のargs[i]をvars[i]に置換する
    // 置換順序は引数の順序通りとする (前から順に置換)
    // 注意: args同士が重複する場合、前の引数が優先して置換される

    var currentBody = target
    args.zip(vars).foreach { case (arg, v) =>
      currentBody = substTerm(currentBody, arg, Expr.Var(v))
    }

    val lambda = vars.foldRight(currentBody) { (name, body) =>
      Expr.App(Expr.Sym("λ"), List(Expr.Var(name), body))
    }

    // 生成された解を追加
    // 注意: 厳密なHOUでは複数の解がありうるが、ここでは「最大抽象化」の1つのみを返す
    LazyList(subst + (id -> lambda))
  }

  /** 高次path対応の出現チェック metaId が expr 内に出現するか、または expr 内に metaId の引数 args
    * に含まれない自由変数が出現するかをチェックします。
    */
  private def occursCheckHigher(
      metaId: MetaId,
      expr: Expr,
      args: List[Expr]
  ): Boolean = {
    def collectMetaVars(e: Expr): Set[MetaId] = e match {
      case Expr.Meta(id)   => Set(id)
      case Expr.App(f, as) => collectMetaVars(f) ++ as.flatMap(collectMetaVars)
      case _               => Set.empty
    }

    // メタ変数の循環参照チェック
    if (collectMetaVars(expr).contains(metaId)) return true

    // 高階の場合の依存性チェック:
    // args が空でない場合、expr 内に出現する変数が args に含まれているかを確認する
    if (args.nonEmpty) {
      def collectFreeVars(e: Expr): Set[String] = e match {
        case Expr.Var(n) => Set(n)
        case Expr.App(f, as) => e match {
          case Expr.App(Expr.Sym("λ"), List(Expr.Var(v), body)) =>
            collectFreeVars(body) - v
          case _ => collectFreeVars(f) ++ as.flatMap(collectFreeVars)
        }
        case _ => Set.empty
      }
      
      val exprVars = collectFreeVars(expr)
      val argVars = args.flatMap(collectFreeVars).toSet
      
      // expr に args に含まれない自由変数がある場合は失敗
      if (exprVars.exists(v => !argVars.contains(v))) {
        logger.log(s"[OCCUR CHECK] Free variables in expr not in args: ${exprVars -- argVars}")
        return true
      }
    }
    
    false
  }

  // Expr.Var用のoccur check
  private def occursCheckVar(varName: String, expr: Expr): Boolean = expr match
    case Expr.Var(n) => n == varName
    case Expr.App(h, args) =>
      occursCheckVar(varName, h) || args.exists(occursCheckVar(varName, _))
    case _ => false

  private def occursCheck(metaId: MetaId, expr: Expr): Boolean = expr match
    case Expr.Meta(id)     => id == metaId
    case Expr.App(h, args) =>
      occursCheck(metaId, h) || args.exists(occursCheck(metaId, _))
    case _ => false
