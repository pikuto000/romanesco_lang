// ==========================================
// ResourceOptimizer.scala
// リソース解析に基づくFree命令の挿入と検証
// ==========================================

package romanesco.Runtime

import romanesco.Solver.core.{Expr, CatRule, Goal, ProverConfig, Prover}

/** リソース最適化器
  * 解析パスでゴミリソースを特定し、Free命令を挿入する。
  * 証明器でFree挿入の正しさを検証する。
  */
class ResourceOptimizer:

  private val analyzer = new ResourceAnalyzer()

  /** バイトコードにFree命令を挿入して返す */
  def optimize(code: Array[Op]): Array[Op] =
    val result = analyzer.analyze(code)
    if result.garbageResources.isEmpty then return code

    // ゴミリソースをトポロジカル順にfree（葉から順に）
    val freeOrder = topoSortForFree(result)

    // Return直前にFree命令を挿入
    insertFrees(code, freeOrder)

  /** ゴミリソースのfree順序を算出（子→親の順） */
  private def topoSortForFree(result: AnalysisResult): List[Int] =
    val garbage = result.garbageResources
    val state = result.finalState

    // ゴミリソース間の依存関係（親→子）を収集
    // 子を先にfreeする必要がある
    val sorted = new scala.collection.mutable.ArrayBuffer[Int]()
    val visited = new scala.collection.mutable.HashSet[Int]()

    def visit(rid: Int): Unit =
      if visited.contains(rid) || !garbage.contains(rid) then return
      visited += rid
      // 子リソースを先に訪問
      state.resources.get(rid) match
        case Some(r) =>
          for child <- r.children if garbage.contains(child) do
            visit(child)
        case None => ()
      sorted += rid

    for rid <- garbage do visit(rid)
    sorted.toList

  /** Return直前にFree命令列を挿入 */
  private def insertFrees(code: Array[Op], freeOrder: List[Int]): Array[Op] =
    val freeOps = freeOrder.map(rid => Op.Free(rid))

    // Return命令の位置を見つける
    val returnIdx = code.lastIndexWhere {
      case Op.Return => true
      case _ => false
    }

    if returnIdx >= 0 then
      // Return直前に挿入
      val (before, after) = code.splitAt(returnIdx)
      before ++ freeOps ++ after
    else
      // Returnが無い場合、末尾に追加
      code ++ freeOps

  // ==========================================
  // 証明器による検証
  // ==========================================

  /** ゴミリソースのfreeが正しいことを証明器で検証する
    * @return Right(freeOrder) 検証成功時、Left(errorMsg) 失敗時
    */
  def verify(code: Array[Op]): Either[String, List[Int]] =
    val result = analyzer.analyze(code)
    if result.garbageResources.isEmpty then
      return Right(Nil)

    // ゴミリソースを線形コンテキストに変換
    val linearCtx = buildLinearContext(result)

    // free規則を構築
    val freeRules = buildFreeRules(result)

    // 証明器で emp を証明
    val goal = Expr.Sym("emp")
    val initialGoal = Goal(
      context = Nil,
      linearContext = linearCtx,
      target = goal
    )

    val config = ProverConfig(
      rules = freeRules,
      pluginPacks = Nil,
      disabledPlugins = Set("all")
    )
    val prover = new Prover(config)

    try {
      val proofResult = prover.prove(
        goal = goal,
        rules = freeRules,
        maxDepth = result.garbageResources.size + 5,
        timeoutMs = 5000,
        initialGoal = Some(initialGoal)
      )
      proofResult match
        case Right(_) =>
          // 証明成功 = 全線形リソースを消費できた = freeが安全
          val freeOrder = topoSortForFree(result)
          Right(freeOrder)
        case Left(_) =>
          Left("証明失敗: ゴミリソースを安全にfreeできることを証明できなかった")
    } catch {
      case e: Exception =>
        Left(s"検証エラー: ${e.getMessage}")
    }

  /** ゴミリソースを線形コンテキストのExprに変換 */
  def buildLinearContext(result: AnalysisResult): List[(String, Expr)] =
    result.garbageDetails.map { r =>
      val kindExpr = r.kind match
        case ResourceKind.Atom => Expr.Sym("Atom")
        case ResourceKind.Shell =>
          if r.children.isEmpty then Expr.Sym("Shell")
          else Expr.App(Expr.Sym("Shell"), r.children.toList.sorted.map(c => Expr.Sym(s"r$c")))
        case ResourceKind.PairStruct =>
          Expr.App(Expr.Sym("Pair"), r.children.toList.sorted.map(c => Expr.Sym(s"r$c")))
        case ResourceKind.InlStruct =>
          Expr.App(Expr.Sym("Inl"), r.children.toList.sorted.map(c => Expr.Sym(s"r$c")))
        case ResourceKind.InrStruct =>
          Expr.App(Expr.Sym("Inr"), r.children.toList.sorted.map(c => Expr.Sym(s"r$c")))
        case ResourceKind.UnitRes => Expr.Sym("UnitRes")

      val prop = Expr.App(Expr.Sym("↦"), List(Expr.Sym(s"r${r.id}"), kindExpr))
      (s"g${r.id}", prop)
    }

  /** free推論規則を構築 */
  def buildFreeRules(result: AnalysisResult): List[CatRule] =
    result.garbageDetails.flatMap { r =>
      r.kind match
        case ResourceKind.Atom =>
          // r ↦ Atom ⊸ emp （線形コンテキストから消費するだけ）
          List(CatRule(
            name = s"free-r${r.id}",
            lhs = Expr.App(Expr.Sym("↦"), List(Expr.Sym(s"r${r.id}"), Expr.Sym("Atom"))),
            rhs = Expr.Sym("emp"),
            domain = "memory"
          ))
        case ResourceKind.Shell =>
          List(CatRule(
            name = s"free-r${r.id}",
            lhs = if r.children.isEmpty then
              Expr.App(Expr.Sym("↦"), List(Expr.Sym(s"r${r.id}"), Expr.Sym("Shell")))
            else
              Expr.App(Expr.Sym("↦"), List(
                Expr.Sym(s"r${r.id}"),
                Expr.App(Expr.Sym("Shell"), r.children.toList.sorted.map(c => Expr.Sym(s"r$c")))
              )),
            rhs = Expr.Sym("emp"),
            domain = "memory"
          ))
        case _ =>
          Nil
    }

