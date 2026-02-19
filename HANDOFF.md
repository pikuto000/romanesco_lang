# プラグイン指向アーキテクチャ リファクタリング 引き継ぎドキュメント

## 現在の進捗

### ✅ Phase 1: Plugin基底トレイト + LogicPlugin継承 — 完了・テスト通過

**実施した変更:**

1. **`scala/Solver/Plugin.scala` (新規作成)**
   - `trait Plugin` — 基底トレイト（name, priority, dependencies, providedRules, providedAlgebras, normalizeHook, isEnabled）
   - `case class NormalizationContext` — 将来の拡張用

2. **`scala/Solver/LogicPlugin.scala` (修正)**
   - `trait LogicPlugin` → `trait LogicPlugin extends Plugin` に変更
   - `def name: String` の定義を削除（Pluginから継承）

3. **全プラグインに `override def priority: Int` を追加:**
   | ファイル | プラグイン | priority |
   |---------|-----------|----------|
   | StandardPlugins.scala | AxiomPlugin | 10 |
   | HoTTSearch.scala | HoTTPlugin | 20 |
   | CubicalPlugin.scala | CubicalPlugin | 21 |
   | StandardPlugins.scala | IntroductionPlugin | 50 |
   | LinearLogicSearch.scala | LinearLogicPlugin | 40 |
   | HoareLogicSearch.scala | HoareLogicPlugin | 41 |
   | PersistentLogicSearch.scala | PersistentLogicPlugin | 60 |
   | ModalLogicSearch.scala | ModalLogicPlugin | 70 |
   | TemporalLogicSearch.scala | TemporalLogicPlugin | 71 |
   | ForwardReasoningSearch.scala | ForwardReasoningPlugin | 80 |
   | InductionPlugin.scala | InductionPlugin | 150 |
   | RewritePlugin.scala | RewritePlugin | 180 |
   | StandardPlugins.scala | UserRulePlugin | 200 |

### ⬜ Phase 2: PluginRegistry + Prover統合 — 未着手

**作成するファイル: `scala/Solver/PluginRegistry.scala`**
```scala
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
}
```

**Prover.scala の修正:**
- L34-48 のハードコード `private val plugins: List[LogicPlugin] = List(...)` を `PluginRegistry.defaultPlugins` に置換
- `backwardRuleIndex` / `forwardRuleIndex` に `plugins.flatMap(_.providedRules)` を追加
- `getAlgebras` に `plugins.flatMap(_.providedAlgebras)` を追加

**重要:** 現在のProverのプラグイン順序は priority順とは若干異なる（IntroductionPlugin が LinearLogicPlugin より前に来ている）。`PluginRegistry.defaultPlugins` の結果が現行の順序を再現するか確認すること。

### ⬜ Phase 3: AlgebraPlugin — 未着手

**作成するファイル: `scala/Solver/AlgebraPlugin.scala`**
```scala
package romanesco.Solver.core

abstract class AlgebraPlugin(val algebra: InitialAlgebra) extends LogicPlugin {
  override def name: String = s"Algebra[${algebra.name}]"
  override def priority: Int = 30
  def computationRules: List[CatRule]
  override def providedRules: List[CatRule] = computationRules
  override def providedAlgebras: List[InitialAlgebra] = List(algebra)
  // getGoalHooks/getContextHooks はデフォルト（空）のまま
}
```

7つの具象プラグイン（同ファイル内）:
- `NatAlgebraPlugin` — `StandardRules.natAlgebra` + `StandardRules.natPlusRules`
- `ListAlgebraPlugin` — `StandardRules.listAlgebra` + `StandardRules.listAppendRules`
- `TreeAlgebraPlugin` — `StandardRules.treeAlgebra` + mirror関連ルール（listAppendRulesに含まれている）
- `S1AlgebraPlugin` — `StandardRules.s1Algebra` + 空ルール
- `MaybeAlgebraPlugin` — `StandardRules.maybeAlgebra` + 空ルール
- `IntervalAlgebraPlugin` — `StandardRules.defaultAlgebras(5)` + `StandardRules.intervalAlgebra`
- `SuspAlgebraPlugin` — `StandardRules.defaultAlgebras(6)` + 空ルール

### ⬜ Phase 4: 正規化プラグイン化 — 未着手

**Rewriter.scala の修正:**
1. `var normalizationPlugins: List[Plugin] = Nil` を `object Rewriter` に追加
2. `def registerNormalizationPlugins(plugins: List[Plugin]): Unit` を追加
3. `builtinRules` の最後（L453 `case _ => expr`）の前に:
   ```scala
   case other =>
     normalizationPlugins.view.flatMap(_.normalizeHook(other)).headOption.getOrElse(other)
   ```
4. `builtinRules` から以下を削除し、対応する AlgebraPlugin の `normalizeHook` に移行:
   - L344-352: Nat演算 (`plus`) → `NatAlgebraPlugin.normalizeHook`
   - L354-368: List演算 (`append`, `reverse`, `mirror`) → `ListAlgebraPlugin.normalizeHook` / `TreeAlgebraPlugin.normalizeHook`
   - L370-372: `map` → `ListAlgebraPlugin.normalizeHook`
   - L374-380: Vector演算 → 新たな `VectorAlgebraPlugin.normalizeHook` または残置
   - L382-417: Monad演算 (`fmap`, `return`, `bind`, `bind_list`, `bind_maybe` 等) → 新たな Monad系プラグインか残置
   - L413-418: `id`, `vmap`, `compose` → 残置が安全

**注意:** `builtinRules` に残すもの:
- HoTT path reduction (L128-256)
- 面制約の正規化 (L262-273)
- Cubical Kan operations (L276-321)
- ラムダ計算 β簡約 (L323-329)
- 圏論的公理 (L331-342)
- 否定の展開 (L422-423)
- 論理的簡約 (L425-436)
- ストリーム的演算 (L438-451)
- list_prop append (L258-260) — これも移行候補

### ⬜ Phase 5: PluginPacks — 未着手

**作成するファイル: `scala/Solver/PluginPacks.scala`**
```scala
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
```

### ⬜ Phase 6: ProverConfig拡張 — 未着手

**Core.scala の `ProverConfig` に追加:**
```scala
case class ProverConfig(
  // ...既存フィールド...
  pluginPacks: List[PluginPack] = Nil,       // 空ならデフォルト全パック
  disabledPlugins: Set[String] = Set.empty
)
```

**Prover.scala の修正:**
- コンストラクタまたは初期化時に `initializePlugins(config)` を呼ぶ
- `initializePlugins`: パック展開 → disabledフィルタ → `PluginRegistry.resolvePluginOrder` → Rewriter登録

## テスト方法

```powershell
# 全テスト
powershell -ExecutionPolicy Bypass -File run.ps1

# CubicalBasicTest（別途）
sbt "runMain romanesco.Solver.SolverTests.CubicalBasicTest"
```

各フェーズ後に必ず全テスト通過を確認すること。

## 重要な注意事項

1. **プラグイン順序が重要:** HoTTPlugin は IntroductionPlugin より前に来る必要がある（isSet加速のため）。priority値でこれは保証済み（HoTT=20 < Introduction=50）。
2. **isProp/isSet は Rewriter の logicalHeads に入れてはいけない**（MEMORY.mdに記載）。
3. **パッケージ:** 全て `romanesco.Solver.core` パッケージ。
4. **外部ライブラリなし:** Scala 3 標準ライブラリのみ使用可。
5. **コメント・変数名は日本語OK。**
6. Phase 4 での `builtinRules` からの移行は、移行前後で `Rewriter.normalize` の結果が同一であることを確認すべき。正規化の適用順序（builtinRules が先、normalizeHook が後）に注意。
