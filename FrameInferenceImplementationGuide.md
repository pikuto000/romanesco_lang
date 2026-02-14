# 🚀 フレーム推論強化版 実装ガイド

## 📊 現状分析

### ✅ 既に実装されている機能
- 基本的なフレーム推論 (`searchFrameInference`)
- 線形論理の探索 (`LinearLogicSearch.scala`)
- 分離論理のテスト（3つのフレーム推論テストケースが成功）

### ⏳ 今回追加する機能
1. **より高度なフレーム推論**
   - 部分的なフレーム（全リソースではなく必要な分だけ）
   - 最小フレームの優先探索

2. **フレームルールの自動適用**
   - `{P} C {Q}` から `{P * F} C {Q * F}` を自動導出

3. **プログラム検証特化機能**
   - リソース使用量の解析
   - 型無しプログラミング言語への適用

---

## 🎯 実装ステップ

### Phase 1: 既存コードの強化（1-2日）

#### ステップ 1.1: LinearLogicSearch.scalaのバックアップ
```bash
cd C:\Users\Cocoa\romanesco\scala\Solver
cp LinearLogicSearch.scala LinearLogicSearch.scala.backup
```

#### ステップ 1.2: searchFrameInferenceの置き換え

**現在のコード（74-91行目）:**
```scala
private def searchFrameInference(
    goal: Expr,
    linearContext: Context,
    subst: Subst
): SolveTree[(ProofTree, Subst, Context)] = {
  if (linearContext.isEmpty) return SolveTree.Failure()
  
  val resources = linearContext.map(_._2)
  val frame = resources.reduceLeft((acc, e) => Expr.App(Expr.Sym(SepAnd), List(acc, e)))
  
  SolveTree.fromLazyList(unify(frame, goal, subst).map { s =>
    (
      ProofTree.Leaf(applySubst(goal, s), "frame-inference"),
      s,
      Nil
    )
  })
}
```

**強化版コード:**
```scala
private def searchFrameInference(
    goal: Expr,
    linearContext: Context,
    subst: Subst
): SolveTree[(ProofTree, Subst, Context)] = {
  if (linearContext.isEmpty) return SolveTree.Failure()
  
  logger.log(s"[Frame Inference] Goal: $goal")
  logger.log(s"[Frame Inference] Resources: ${linearContext.map(_._2).mkString(", ")}")
  
  // 戦略1: 全リソースをフレームに（既存の動作）
  val fullFrame = {
    val resources = linearContext.map(_._2)
    val frame = resources.reduceLeft((acc, e) => 
      Expr.App(Expr.Sym(SepAnd), List(acc, e))
    )
    
    SolveTree.fromLazyList(unify(frame, goal, subst).map { s =>
      logger.log(s"[Frame] Full: $frame")
      (
        ProofTree.Leaf(applySubst(goal, s), "frame-full"),
        s,
        Nil
      )
    })
  }
  
  // 戦略2: 最小フレームを優先（新規）
  val minimalFrames = (1 until linearContext.length).toList.flatMap { n =>
    linearContext.combinations(n).map { selected =>
      val frame = selected.map(_._2).reduceLeft((acc, e) => 
        Expr.App(Expr.Sym(SepAnd), List(acc, e))
      )
      val remaining = linearContext.diff(selected)
      
      SolveTree.fromLazyList(unify(frame, goal, subst).map { s =>
        logger.log(s"[Frame] Minimal ($n): $frame")
        (
          ProofTree.Leaf(applySubst(goal, s), s"frame-min-$n"),
          s,
          remaining
        )
      })
    }
  }
  
  // 最小を優先
  if (minimalFrames.nonEmpty) {
    SolveTree.merge(minimalFrames :+ fullFrame)
  } else {
    fullFrame
  }
}
```

**変更点:**
1. ✅ ログ追加（デバッグ用）
2. ✅ 最小フレームの探索
3. ✅ 複数戦略のマージ

---

### Phase 2: フレームルールの追加（2-3日）

#### ステップ 2.1: 新しいメソッドを追加

`LinearLogicSearch.scala`に以下を追加（`searchFrameInference`の後）:

```scala
/** フレームルールの自動適用
  * {P} C {Q} から {P * F} C {Q * F} を導出
  */
private def searchFrameRule(
    goal: Expr,
    rules: List[CatRule],
    context: Context,
    linearContext: Context,
    subst: Subst,
    depth: Int,
    limit: Int,
    visited: Set[(Expr, Set[Expr], List[Expr])],
    raaCount: Int,
    inductionCount: Int,
    guarded: Boolean,
    history: List[Expr]
): SolveTree[(ProofTree, Subst, Context)] = {
  goal match {
    case App(Sym(LImplies), List(
      App(Sym(SepAnd), List(p, framePre)),
      App(Sym(SepAnd), List(q, framePost))
    )) =>
      unify(framePre, framePost, subst).headOption match {
        case Some(s1) =>
          val frameHyp = (s"frame_$depth", applySubst(framePre, s1))
          
          searchLinearImpliesGoal(
            App(Sym(LImplies), List(p, q)),
            p, q, rules, context,
            frameHyp :: linearContext,
            s1, depth + 1, limit, visited,
            raaCount, inductionCount, guarded, history
          ).map { case (innerProof, s2, restL) =>
            (
              ProofTree.Node(
                applySubst(goal, s2),
                "frame-rule",
                List(innerProof)
              ),
              s2,
              restL.filterNot(_._1.startsWith("frame_"))
            )
          }
        case None => SolveTree.Failure()
      }
    case _ => SolveTree.Failure()
  }
}
```

#### ステップ 2.2: getGoalHooksを更新

`getGoalHooks`メソッドの`LImplies`ケースを修正:

```scala
case Expr.App(Expr.Sym(LImplies), List(a, b)) =>
  List(
    searchLinearImpliesGoal(goal, a, b, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history),
    // 追加: フレームルール
    searchFrameRule(goal, rules, context, linearContext, subst, depth, limit, visited, raaCount, inductionCount, guarded, history)
  )
```

---

### Phase 3: テストの追加（1日）

#### ステップ 3.1: 新しいテストファイルを作成

`C:\Users\Cocoa\romanesco\scala\Solver\SolverTests\EnhancedFrameTest.scala`:

```scala
package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object EnhancedFrameTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val rules = StandardRules.all
    val config = ProverConfig(rules = rules, maxComplexity = 300)
    val prover = new Prover(config)

    println("=== Enhanced Frame Inference Test ===")

    val testCases = List(
      // 既存のテスト（動作確認）
      ("∃F. (A * B ⊸ A * F)", true, 10, "Basic frame"),
      ("∃F. (x ↦ 1 * y ↦ 2 ⊸ x ↦ 1 * F)", true, 10, "Points-to frame"),
      ("∃F. (A * B * C ⊸ B * F)", true, 10, "Multi-resource frame"),
      
      // 新規テスト（最小フレーム）
      ("∃F. (A * B * C * D ⊸ A * B * F)", true, 15, "Minimal frame (C * D)"),
      ("∃F. (x ↦ 1 * y ↦ 2 * z ↦ 3 ⊸ y ↦ 2 * F)", true, 15, "Minimal frame (x ↦ 1 * z ↦ 3)"),
      
      // フレームルール
      ("(x ↦ 1 ⊸ x ↦ 2) → (x ↦ 1 * y ↦ 3 ⊸ x ↦ 2 * y ↦ 3)", true, 20, "Frame rule application"),
      
      // プログラム検証風
      ("∃F. ({file_open(f) * buffer(b)} read(f) {file_open(f) * F})", true, 15, "File handle frame")
    )

    testCases.foreach { case (input, expected, depth, description) =>
      print(s"Case: $description ... ")
      try {
        val goal = TestParser.parse(input)
        val result = prover.prove(goal, maxDepth = depth)
        result match {
          case Right(_) =>
            if (expected) println("✓ OK")
            else println("✗ FAIL (Should have failed)")
          case Left(trace) =>
            if (!expected) println("✓ OK (Failed as expected)")
            else println(s"✗ FAIL: ${trace.reason}")
        }
      } catch {
        case e: Exception => println(s"Error: ${e.getMessage}")
      }
    }
  }
}
```

#### ステップ 3.2: テスト実行

```bash
sbt "run romanesco.Solver.SolverTests.EnhancedFrameTest"
```

---

### Phase 4: プログラム検証への統合（3-5日）

#### ステップ 4.1: リソース述語の定義

`StandardRules.scala`に追加:

```scala
// Hoare論理の後に追加
val resourceRules = List(
  // ファイル操作
  CatRule("file-open", 
    sym("open")(sym("file_exists")(v("f"))), 
    sym("file_open")(v("f"))),
  
  CatRule("file-close",
    sym("close")(sym("file_open")(v("f"))),
    sym("⊤")),
  
  CatRule("file-read",
    sym("read")(sym("file_open")(v("f"))),
    sym("file_open")(v("f"))),  // ファイルは開いたまま
  
  // メモリ操作
  CatRule("memory-alloc",
    sym("malloc")(v("size")),
    sym("↦")(v("ptr"), sym("_"))),
  
  CatRule("memory-free",
    sym("free")(sym("↦")(v("ptr"), v("val"))),
    sym("⊤")),
  
  // ロック操作
  CatRule("lock-acquire",
    sym("acquire")(sym("lock_free")(v("l"))),
    sym("lock_held")(v("l"))),
  
  CatRule("lock-release",
    sym("release")(sym("lock_held")(v("l"))),
    sym("lock_free")(v("l")))
)

// allに追加
val all: List[CatRule] = products ++ coproducts ++ exponentials ++ colimits ++ equality ++ modal ++ linear ++ separation ++ hott ++ natPlusRules ++ listAppendRules ++ resourceRules
```

#### ステップ 4.2: プログラム検証例

```scala
object ProgramVerificationExample {
  def main(args: Array[String]): Unit = {
    val rules = StandardRules.all
    val config = ProverConfig(rules = rules)
    val prover = new Prover(config)
    
    // 例: ファイル処理プログラム
    val program = """
      {file_exists(f) * buffer(b)}
      open(f);
      read(f);
      close(f)
      {buffer(b)}
    """
    
    val goal = TestParser.parse(program)
    prover.prove(goal, maxDepth = 25) match {
      case Right(proof) =>
        println("✓ プログラムは安全です")
        println(proof.tree.format(0))
      case Left(trace) =>
        println(s"✗ 検証失敗: ${trace.reason}")
        println(trace.format())
    }
  }
}
```

---

## 📊 期待される成果

### Phase 1完了後
- ✅ 最小フレームの推論が動作
- ✅ より詳細なログ出力
- ✅ 既存のテストが全て成功

### Phase 2完了後
- ✅ フレームルールの自動適用
- ✅ `{P} C {Q}` から `{P * F} C {Q * F}` を自動導出
- ✅ より柔軟なリソース管理

### Phase 3完了後
- ✅ 新しいテストケースが全て成功
- ✅ 複雑なフレーム推論が動作
- ✅ パフォーマンスの確認

### Phase 4完了後
- ✅ 実用的なプログラム検証が可能
- ✅ ファイル、メモリ、ロックのリソース管理
- ✅ 型無しプログラミング言語の静的検証

---

## 🐛 予想されるトラブルと解決策

### 問題1: コンパイルエラー

**症状:**
```
error: not found: value logger
```

**解決策:**
```scala
import romanesco.Utils.Debug.logger
```

### 問題2: テストが失敗

**症状:**
```
✗ FAIL: No proof found
```

**解決策:**
1. `maxDepth`を増やす（15 → 20 → 25）
2. ログを有効化して探索を確認
   ```scala
   logger.switch(true)
   logger.setMaxDepth(10)
   ```

### 問題3: パフォーマンスが遅い

**症状:**
テストに30秒以上かかる

**解決策:**
1. `maxComplexity`を調整
2. `maxParallelism`を増やす
3. 最小フレームの探索範囲を制限

---

## 📚 提供ファイル

1. ✅ `AdvancedFrameInference.scala` - 高度なフレーム推論の実装
2. ✅ `TypelessLanguageVerificationGuide.md` - 使用ガイド
3. ✅ `EnhancedLinearLogicSearch.scala` - 強化版のコード例
4. ✅ このガイド - 実装手順

---

## 🚀 次のアクション

### 今すぐ（30分）
1. LinearLogicSearch.scalaのバックアップ
2. `searchFrameInference`を強化版に置き換え
3. コンパイル確認

### 今日中（2-3時間）
1. `searchFrameRule`メソッドの追加
2. `getGoalHooks`の更新
3. 基本テストの実行

### 今週中（1-2日）
1. 新しいテストケースの追加
2. 全テストの実行
3. パフォーマンスの最適化

### 来週（3-5日）
1. リソース述語の追加
2. プログラム検証例の作成
3. ドキュメント整備

---

**準備完了！実装を始めましょう！** 🎉

何か質問や問題があれば、遠慮なく聞いてください！
