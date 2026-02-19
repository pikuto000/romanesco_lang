package romanesco.Solver.core

object PluginRegistry {
  /** 依存関係をトポロジカルソートし、同レベル内は priority でソートする */
  def resolvePluginOrder(plugins: List[Plugin]): List[Plugin] = {
    val byName = plugins.map(p => p.name -> p).toMap
    val visited = scala.collection.mutable.Set[String]()
    val result = scala.collection.mutable.ListBuffer[Plugin]()
    def visit(p: Plugin): Unit = {
      if (visited.contains(p.name)) return
      visited += p.name
      p.dependencies.foreach { dep => byName.get(dep).foreach(visit) }
      result += p
    }
    // Priorityが低い（数値が小さい）順に訪れる。
    // トポロジカルソートの結果、依存先が前に来る。
    plugins.sortBy(_.priority).foreach(visit)
    result.toList
  }

  /** デフォルトの全LogicPluginリスト（priority順） */
  def defaultPlugins: List[LogicPlugin] = {
    val plugins: List[LogicPlugin] = List(
      new AxiomPlugin(), new HoTTPlugin(), new CubicalPlugin(),
      new IntroductionPlugin(), new LinearLogicPlugin(), new HoareLogicPlugin(),
      new PersistentLogicPlugin(), new ForwardReasoningPlugin(),
      new ModalLogicPlugin(), new TemporalLogicPlugin(),
      new UserRulePlugin(), new InductionPlugin(), new RewritePlugin()
    )
    resolvePluginOrder(plugins).collect { case p: LogicPlugin => p }
  }

  /** 全ての組み込みプラグイン（AlgebraPluginを含む） */
  def allPlugins: List[Plugin] = {
    val plugins: List[Plugin] = List(
      new AxiomPlugin(), new HoTTPlugin(), new CubicalPlugin(),
      new IntroductionPlugin(), new LinearLogicPlugin(), new HoareLogicPlugin(),
      new PersistentLogicPlugin(), new ForwardReasoningPlugin(),
      new ModalLogicPlugin(), new TemporalLogicPlugin(),
      new UserRulePlugin(), new InductionPlugin(), new RewritePlugin(),
      new NatAlgebraPlugin(), new ListAlgebraPlugin(), new TreeAlgebraPlugin(),
      new S1AlgebraPlugin(), new MaybeAlgebraPlugin(), new IntervalAlgebraPlugin(),
      new SuspAlgebraPlugin()
    )
    resolvePluginOrder(plugins)
  }
}
