package romanesco.Solver.core

trait PluginPack {
  def name: String
  def dependencies: List[String] = Nil
  def plugins: List[LogicPlugin]
}

object StandardPluginPack extends PluginPack {
  def name = "Standard"
  def plugins = List(new AxiomPlugin(), new IntroductionPlugin(), new PersistentLogicPlugin(), new UserRulePlugin())
}

object HoTTPluginPack extends PluginPack {
  def name = "HoTT"
  override def dependencies = List("Standard")
  def plugins = List(new HoTTPlugin(), new CubicalPlugin())
}

object ResourceLogicPack extends PluginPack {
  def name = "ResourceLogic"
  override def dependencies = List("Standard")
  def plugins = List(new LinearLogicPlugin(), new HoareLogicPlugin())
}

object ModalTemporalPack extends PluginPack {
  def name = "ModalTemporal"
  override def dependencies = List("Standard")
  def plugins = List(new ModalLogicPlugin(), new TemporalLogicPlugin())
}

object AdvancedReasoningPack extends PluginPack {
  def name = "AdvancedReasoning"
  override def dependencies = List("Standard")
  def plugins = List(new ForwardReasoningPlugin(), new InductionPlugin(), new RewritePlugin())
}

object AlgebraPluginPack extends PluginPack {
  def name = "Algebra"
  override def dependencies = List("Standard")
  def plugins = List(
    new NatAlgebraPlugin(), new ListAlgebraPlugin(), new TreeAlgebraPlugin(),
    new S1AlgebraPlugin(), new MaybeAlgebraPlugin(), new IntervalAlgebraPlugin(),
    new SuspAlgebraPlugin()
  )
}
