# 証明器の問題分析と解決策

## 発見された問題

### 1. Occur Check（循環参照チェック）の欠如

**問題:**
ユニフィケーション時に、変数が自分自身を含む項と単一化しようとしている。

**例:**
```
[DEBUG] Unifying: n_0 with S(n_0)
[DEBUG] Unifying: m_0 with plus(0,S(m_0))
```

**解決策:**
```scala
def unify(term1: Term, term2: Term, subst: Substitution): Option[Substitution] = {
  (term1, term2) match {
    case (Var(x), t) => 
      if (t == Var(x)) Some(subst)
      else if (occursIn(x, t)) None  // ← この行を追加
      else Some(subst + (x -> t))
    case (t, Var(x)) => 
      if (occursIn(x, t)) None  // ← この行を追加
      else Some(subst + (x -> t))
    // ... 他のケース
  }
}

// Occur checkの実装
def occursIn(v: String, term: Term): Boolean = term match {
  case Var(x) => x == v
  case App(f, args) => args.exists(occursIn(v, _))
  case Lam(param, body) => param != v && occursIn(v, body)
  case _ => false
}
```

### 2. 帰納法の証明戦略の問題

**問題:**
基本的な帰納法の定理が深さ0で失敗している。

**失敗例:**
```
✗ (Depth 0) Goal: ∀(n,plus(n,0) = n)
```

**原因:**
- 帰納法のタクティックが適切に起動されていない
- ∀除去と帰納法の適用順序が正しくない

**解決策:**
```scala
def proveByInduction(goal: Term, maxDepth: Int): ProofResult = {
  goal match {
    case Forall(v, body) =>
      // ステップ1: ∀を導入（fresh変数を作る）
      val freshVar = freshVariable(v)
      val instantiatedGoal = substitute(body, v, freshVar)
      
      instantiatedGoal match {
        case Equality(lhs, rhs) if containsInductiveType(lhs) || containsInductiveType(rhs) =>
          // ステップ2: 帰納法を適用
          val baseCase = substitute(instantiatedGoal, freshVar, baseConstructor)
          val inductiveCase = substitute(instantiatedGoal, freshVar, inductiveConstructor(freshVar))
          
          // ステップ3: 両方のケースを証明
          for {
            _ <- prove(baseCase, maxDepth - 1)
            _ <- prove(inductiveCase, maxDepth - 1, inductiveHypothesis)
          } yield ProofSuccess
        case _ => 
          prove(instantiatedGoal, maxDepth - 1)
      }
    case _ => 
      // 帰納法以外の証明戦略
      proveDirectly(goal, maxDepth)
  }
}
```

### 3. 探索戦略の最適化

**問題:**
- 同じ状態に3,425回も訪問している
- 結合律の証明だけで5万行のログ

**解決策:**

#### 3-1. 訪問済み状態の管理を改善
```scala
class ProofSearch {
  private val visitedStates = mutable.Set[NormalizedGoal]()
  
  def search(goal: Goal, depth: Int): Option[Proof] = {
    val normalized = normalize(goal)
    
    // 既に訪問した状態ならスキップ
    if (visitedStates.contains(normalized)) {
      logger.debug(s"[PRUNED] Already visited: $normalized")
      return None
    }
    
    visitedStates += normalized
    // ... 証明を試みる
  }
  
  // ゴールを正規化（α変換等価な項を同一視）
  def normalize(goal: Goal): NormalizedGoal = {
    alphaRename(goal).canonicalize()
  }
}
```

#### 3-2. より賢い探索順序
```scala
def searchStrategies(goal: Goal): List[Tactic] = goal match {
  // 簡単な証明から試す
  case Equality(t1, t2) if t1 == t2 => 
    List(Reflexivity)  // 最初に反射律
    
  case Forall(_, _) => 
    List(Introduction, Induction)  // ∀は導入か帰納法
    
  case Implication(_, _) => 
    List(Introduction, ModusPonens)  // →は導入か前向き推論
    
  case _ => 
    standardTactics  // 一般的なタクティック
}

def prove(goal: Goal, maxDepth: Int): Option[Proof] = {
  if (maxDepth == 0) return None
  
  // 優先度の高い戦略から順に試す
  searchStrategies(goal).collectFirst {
    case tactic if applyTactic(tactic, goal, maxDepth).isDefined =>
      applyTactic(tactic, goal, maxDepth)
  }
}
```

#### 3-3. ログレベルの調整
```scala
// DEBUGログを条件付きで出力
def debug(msg: => String): Unit = {
  if (logger.isDebugEnabled && depth <= debugLogDepthLimit) {
    logger.debug(msg)
  }
}

// 推奨設定
val debugLogDepthLimit = 3  // 深さ3以下だけ詳細ログ
```

## テスト結果の改善予測

修正前:
```
✗ Failed to prove: ∀(n,plus(n,0) = n)  [Depth 0]
✗ Failed to prove: ∀(n,∀(m,plus(n,S(m)) = S(plus(n,m))))  [Depth 0]
```

修正後（期待される結果）:
```
✓ Proved: ∀(n,plus(n,0) = n)  [By induction on n]
  Base case: plus(0,0) = 0  [By computation]
  Inductive step: plus(S(n),0) = S(n)  [By IH and computation]
✓ Proved: ∀(n,∀(m,plus(n,S(m)) = S(plus(n,m))))  [By induction on n]
```

## 優先度

1. **最優先**: Occur checkの実装（これがないと多くの証明が無限ループする）
2. **高優先**: 帰納法戦略の修正（基本的な定理が証明できない）
3. **中優先**: 探索戦略の最適化（パフォーマンス改善）

## 実装チェックリスト

- [ ] `unify`関数に`occursIn`チェックを追加
- [ ] 帰納法タクティックの実装を確認・修正
- [ ] ∀導入と帰納法の適用順序を修正
- [ ] 訪問済み状態の管理を改善（正規化を含む）
- [ ] タクティックの優先順位付けを実装
- [ ] デバッグログの出力を制限


```scala
// ==============================================
// 証明器の修正コードサンプル
// ==============================================

// ----------------------------------------
// 1. Occur Checkの実装
// ----------------------------------------

object Unification {
  
  /**
   * Occur check: 変数vがterm内に出現するかチェック
   * これにより無限ループを防ぐ
   */
  def occursIn(v: String, term: Term): Boolean = term match {
    case Var(x) => x == v
    case App(f, args) => 
      occursIn(v, f) || args.exists(occursIn(v, _))
    case Lam(param, body) => 
      param != v && occursIn(v, body)
    case Forall(param, body) => 
      param != v && occursIn(v, body)
    case Exists(param, body) => 
      param != v && occursIn(v, body)
    case Equality(lhs, rhs) => 
      occursIn(v, lhs) || occursIn(v, rhs)
    case _ => false
  }

  /**
   * 改善されたunify関数
   */
  def unify(term1: Term, term2: Term, subst: Substitution): Option[Substitution] = {
    val t1 = subst.apply(term1)
    val t2 = subst.apply(term2)
    
    (t1, t2) match {
      // 同じ項
      case (t, s) if t == s => 
        Some(subst)
      
      // 変数と項の単一化
      case (Var(x), t) => 
        if (occursIn(x, t)) {
          // Occur check失敗
          logger.debug(s"[OCCUR CHECK FAILED] Cannot unify $x with $t")
          None
        } else {
          Some(subst.extend(x, t))
        }
      
      case (t, Var(x)) => 
        if (occursIn(x, t)) {
          logger.debug(s"[OCCUR CHECK FAILED] Cannot unify $x with $t")
          None
        } else {
          Some(subst.extend(x, t))
        }
      
      // 関数適用の単一化
      case (App(f1, args1), App(f2, args2)) if args1.length == args2.length =>
        unify(f1, f2, subst).flatMap { s1 =>
          (args1 zip args2).foldLeft[Option[Substitution]](Some(s1)) {
            case (Some(s), (a1, a2)) => unify(a1, a2, s)
            case (None, _) => None
          }
        }
      
      // その他は失敗
      case _ => 
        logger.debug(s"[UNIFY FAILED] Cannot unify $t1 with $t2")
        None
    }
  }
}

// ----------------------------------------
// 2. 帰納法タクティックの改善
// ----------------------------------------

object InductionTactic {
  
  /**
   * 帰納法を適用するタクティック
   */
  def applyInduction(goal: Term, maxDepth: Int): ProofResult = {
    goal match {
      case Forall(v, body) =>
        logger.info(s"Applying induction on variable: $v")
        
        // ステップ1: 変数を fresh にする
        val freshVar = freshVariable(v)
        val instantiated = substitute(body, v, Var(freshVar))
        
        // ステップ2: 帰納的な型を特定
        getInductiveType(freshVar) match {
          case Some(inductiveType) =>
            // ステップ3: 基底ケースと帰納ステップを生成
            val constructors = getConstructors(inductiveType)
            
            // 基底ケース（引数を取らないコンストラクタ）
            val baseCases = constructors.filter(_.arity == 0).map { constructor =>
              substitute(instantiated, freshVar, constructor.term)
            }
            
            // 帰納ステップ（引数を取るコンストラクタ）
            val inductiveCases = constructors.filter(_.arity > 0).map { constructor =>
              val (inductiveHyp, stepGoal) = makeInductiveStep(
                instantiated, 
                freshVar, 
                constructor
              )
              (inductiveHyp, stepGoal)
            }
            
            // ステップ4: すべてのケースを証明
            for {
              _ <- baseCases.traverse(bc => prove(bc, maxDepth - 1))
              _ <- inductiveCases.traverse { case (hyp, goal) =>
                proveWithHypothesis(goal, hyp, maxDepth - 1)
              }
            } yield ProofSuccess(s"Proved by induction on $v")
          
          case None =>
            // 帰納的型でない場合は通常の証明を試みる
            logger.debug(s"$freshVar is not an inductive type, trying direct proof")
            prove(instantiated, maxDepth - 1)
        }
      
      case _ =>
        logger.debug("Goal is not a universal quantification")
        ProofFailure("Cannot apply induction to non-universal goal")
    }
  }
  
  /**
   * 帰納ステップを生成
   */
  private def makeInductiveStep(
    body: Term, 
    inductiveVar: String, 
    constructor: Constructor
  ): (Term, Term) = {
    val recursiveArgs = constructor.args.filter(isRecursive)
    
    // 帰納法の仮定: 各再帰的引数について body が成り立つ
    val inductiveHypotheses = recursiveArgs.map { arg =>
      substitute(body, inductiveVar, arg)
    }
    
    // 目標: コンストラクタを適用した項について body が成り立つ
    val constructedTerm = constructor.apply(
      constructor.args.map {
        case RecursiveArg(v) => Var(v)
        case NonRecursiveArg(v) => Var(v)
      }
    )
    val stepGoal = substitute(body, inductiveVar, constructedTerm)
    
    val hypothesis = if (inductiveHypotheses.length == 1) {
      inductiveHypotheses.head
    } else {
      And(inductiveHypotheses)
    }
    
    (hypothesis, stepGoal)
  }
}

// ----------------------------------------
// 3. 探索戦略の改善
// ----------------------------------------

object ImprovedProofSearch {
  
  // 訪問済み状態を記録（正規化された形で）
  private val visited = mutable.Set[CanonicalGoal]()
  
  /**
   * ゴールを正規形に変換
   * α同値な項を同一視するため
   */
  def canonicalize(goal: Term): CanonicalGoal = {
    val renamed = alphaRename(goal, Map.empty, 0)._1
    CanonicalGoal(renamed)
  }
  
  /**
   * α変換（変数名を標準化）
   */
  private def alphaRename(
    term: Term, 
    env: Map[String, String], 
    counter: Int
  ): (Term, Int) = term match {
    case Var(x) => 
      (Var(env.getOrElse(x, x)), counter)
    
    case Forall(v, body) =>
      val newVar = s"v$counter"
      val (newBody, newCounter) = alphaRename(body, env + (v -> newVar), counter + 1)
      (Forall(newVar, newBody), newCounter)
    
    case App(f, args) =>
      val (newF, c1) = alphaRename(f, env, counter)
      val (newArgs, c2) = args.foldLeft((List.empty[Term], c1)) {
        case ((acc, c), arg) =>
          val (newArg, newC) = alphaRename(arg, env, c)
          (acc :+ newArg, newC)
      }
      (App(newF, newArgs), c2)
    
    case _ => (term, counter)
  }
  
  /**
   * 改善された証明探索
   */
  def proveWithPruning(goal: Term, maxDepth: Int): ProofResult = {
    // 深さ制限チェック
    if (maxDepth <= 0) {
      return ProofFailure("Depth limit reached")
    }
    
    // 既に訪問した状態かチェック
    val canonical = canonicalize(goal)
    if (visited.contains(canonical)) {
      logger.debug(s"[PRUNED] Already visited: $goal")
      return ProofFailure("Already visited state")
    }
    visited += canonical
    
    // 優先順位付けされたタクティックを試す
    val tactics = prioritizeTactics(goal)
    
    tactics.collectFirst {
      case tactic if applyTactic(tactic, goal, maxDepth).isSuccess =>
        applyTactic(tactic, goal, maxDepth)
    }.getOrElse {
      ProofFailure("No tactic succeeded")
    }
  }
  
  /**
   * ゴールに応じてタクティックの優先順位を決定
   */
  def prioritizeTactics(goal: Term): List[Tactic] = goal match {
    // 等式で両辺が同じ → 反射律で即証明
    case Equality(t1, t2) if t1 == t2 =>
      List(Reflexivity)
    
    // ∀量化 → 導入か帰納法
    case Forall(v, body) if isInductiveVariable(v, body) =>
      List(Induction, ForallIntro)
    
    case Forall(_, _) =>
      List(ForallIntro, Induction)
    
    // 含意 → 導入
    case Implication(_, _) =>
      List(ImplicationIntro, ModusPonens)
    
    // 連言 → 分割
    case And(_, _) =>
      List(AndIntro, Split)
    
    // その他
    case _ =>
      standardTactics
  }
}

// ----------------------------------------
// 4. ログ出力の最適化
// ----------------------------------------

object OptimizedLogger {
  
  var currentDepth: Int = 0
  val maxLogDepth: Int = 3  // この深さまでしか詳細ログを出さない
  
  def debug(msg: => String): Unit = {
    if (currentDepth <= maxLogDepth) {
      println(s"${"  " * currentDepth}[DEBUG] $msg")
    }
  }
  
  def info(msg: => String): Unit = {
    println(s"${"  " * currentDepth}[INFO] $msg")
  }
  
  def withDepth[A](f: => A): A = {
    currentDepth += 1
    try {
      f
    } finally {
      currentDepth -= 1
    }
  }
  
  /**
   * 統計情報のみを記録するモード
   */
  def logStatistics(stats: ProofStatistics): Unit = {
    println(s"""
      |Proof Statistics:
      |  Total unifications: ${stats.unificationCount}
      |  Successful unifications: ${stats.successfulUnifications}
      |  Pruned states: ${stats.prunedStates}
      |  Max depth reached: ${stats.maxDepth}
      |  Time elapsed: ${stats.timeMs}ms
    """.stripMargin)
  }
}

// ----------------------------------------
// 使用例
// ----------------------------------------

object ProofSystemExample {
  def main(args: Array[String]): Unit = {
    // テスト: plus(n, 0) = n
    val goal = Forall("n", 
      Equality(
        App(Var("plus"), List(Var("n"), Var("0"))),
        Var("n")
      )
    )
    
    println("=== Testing improved proof system ===")
    println(s"Goal: $goal")
    println()
    
    val result = ImprovedProofSearch.proveWithPruning(goal, maxDepth = 15)
    
    result match {
      case ProofSuccess(msg) =>
        println(s"✓ Proof succeeded: $msg")
      case ProofFailure(reason) =>
        println(s"✗ Proof failed: $reason")
    }
  }
}

// ==============================================
// 問題を再現する最小テストケース
// ==============================================

object ProofSystemTests {
  
  // ----------------------------------------
  // テスト1: Occur Check の欠如
  // ----------------------------------------
  
  def testOccurCheckProblem(): Unit = {
    println("=== Test 1: Occur Check Problem ===")
    
    // 問題のある単一化: n_0 と S(n_0)
    val var_n = Var("n_0")
    val term_S_n = App(Var("S"), List(Var("n_0")))
    
    println(s"Attempting to unify: $var_n with $term_S_n")
    
    // Occur checkなしの場合（現在の実装）
    val resultWithoutCheck = unifyWithoutOccurCheck(var_n, term_S_n, Substitution.empty)
    resultWithoutCheck match {
      case Some(subst) =>
        println(s"✗ BAD: Unification succeeded (should have failed!)")
        println(s"  Substitution: $subst")
        println(s"  This creates infinite term: n_0 = S(n_0) = S(S(n_0)) = ...")
      case None =>
        println(s"✓ GOOD: Unification correctly failed")
    }
    
    // Occur checkありの場合（修正版）
    val resultWithCheck = unifyWithOccurCheck(var_n, term_S_n, Substitution.empty)
    resultWithCheck match {
      case Some(subst) =>
        println(s"✗ BAD: Unification succeeded (should have failed!)")
      case None =>
        println(s"✓ GOOD: Occur check prevented infinite term")
    }
    
    println()
  }
  
  // ----------------------------------------
  // テスト2: 帰納法の証明
  // ----------------------------------------
  
  def testInductionProblem(): Unit = {
    println("=== Test 2: Induction Problem ===")
    
    // 目標: ∀n. plus(n, 0) = n
    val goal = Forall("n", 
      Equality(
        App(Var("plus"), List(Var("n"), Var("0"))),
        Var("n")
      )
    )
    
    println(s"Goal: $goal")
    println()
    
    // 現在の実装での証明試行
    println("Current implementation:")
    val resultCurrent = proveCurrentVersion(goal, maxDepth = 5)
    resultCurrent match {
      case ProofSuccess(msg) =>
        println(s"✓ Proof succeeded: $msg")
      case ProofFailure(reason) =>
        println(s"✗ Proof failed at depth 0")
        println(s"  Reason: $reason")
        println(s"  Expected: Should apply induction on n")
    }
    println()
    
    // 改善版での証明試行
    println("Improved implementation:")
    val resultImproved = proveImprovedVersion(goal, maxDepth = 5)
    resultImproved match {
      case ProofSuccess(msg) =>
        println(s"✓ Proof succeeded: $msg")
        println(s"  Base case: plus(0, 0) = 0  [by definition]")
        println(s"  Inductive step: plus(S(n), 0) = S(n)  [by IH]")
      case ProofFailure(reason) =>
        println(s"✗ Proof failed: $reason")
    }
    println()
  }
  
  // ----------------------------------------
  // テスト3: 探索の効率性
  // ----------------------------------------
  
  def testSearchEfficiency(): Unit = {
    println("=== Test 3: Search Efficiency ===")
    
    // 複雑な目標: 結合律
    val goal = Forall("x", Forall("y", Forall("z",
      Equality(
        App(Var("concat"), List(
          App(Var("concat"), List(Var("p"), Var("q"))),
          Var("r")
        )),
        App(Var("concat"), List(
          Var("p"),
          App(Var("concat"), List(Var("q"), Var("r")))
        ))
      )
    )))
    
    println(s"Goal: Associativity of concat")
    println()
    
    // 現在の実装
    println("Current implementation:")
    val (resultCurrent, statsCurrent) = proveWithStats(goal, maxDepth = 15)
    println(s"  States visited: ${statsCurrent.statesVisited}")
    println(s"  States pruned: ${statsCurrent.statesPruned}")
    println(s"  Unifications: ${statsCurrent.unifications}")
    println(s"  Log lines: ${statsCurrent.logLines}")
    println(s"  Time: ${statsCurrent.timeMs}ms")
    
    if (statsCurrent.logLines > 10000) {
      println(s"  ⚠ WARNING: Too many log lines (${statsCurrent.logLines})")
    }
    if (statsCurrent.statesPruned > 1000) {
      println(s"  ⚠ WARNING: Too many pruned states (${statsCurrent.statesPruned})")
    }
    println()
    
    // 改善版
    println("Improved implementation:")
    val (resultImproved, statsImproved) = proveWithStatsImproved(goal, maxDepth = 15)
    println(s"  States visited: ${statsImproved.statesVisited}")
    println(s"  States pruned: ${statsImproved.statesPruned}")
    println(s"  Unifications: ${statsImproved.unifications}")
    println(s"  Log lines: ${statsImproved.logLines}")
    println(s"  Time: ${statsImproved.timeMs}ms")
    
    val improvement = ((statsCurrent.logLines - statsImproved.logLines).toDouble / 
                       statsCurrent.logLines * 100).toInt
    println(s"  ✓ Log reduction: $improvement%")
    println()
  }
  
  // ----------------------------------------
  // ヘルパー関数の実装例
  // ----------------------------------------
  
  def unifyWithoutOccurCheck(t1: Term, t2: Term, subst: Substitution): Option[Substitution] = {
    // 現在の実装（occur checkなし）
    (t1, t2) match {
      case (Var(x), t) if !subst.contains(x) =>
        // ⚠ occur check がない！
        Some(subst.extend(x, t))
      case _ => None
    }
  }
  
  def unifyWithOccurCheck(t1: Term, t2: Term, subst: Substitution): Option[Substitution] = {
    // 改善版（occur checkあり）
    (t1, t2) match {
      case (Var(x), t) if !subst.contains(x) =>
        if (occursIn(x, t)) {
          // occur check で拒否
          None
        } else {
          Some(subst.extend(x, t))
        }
      case _ => None
    }
  }
  
  def occursIn(v: String, term: Term): Boolean = term match {
    case Var(x) => x == v
    case App(_, args) => args.exists(occursIn(v, _))
    case _ => false
  }
  
  // ----------------------------------------
  // メイン
  // ----------------------------------------
  
  def main(args: Array[String]): Unit = {
    println("=" * 60)
    println("Proof System Problem Demonstration")
    println("=" * 60)
    println()
    
    testOccurCheckProblem()
    testInductionProblem()
    testSearchEfficiency()
    
    println("=" * 60)
    println("Summary of Problems:")
    println("1. ✗ Occur check is missing")
    println("2. ✗ Induction tactic fails at depth 0")
    println("3. ✗ Search generates excessive logs")
    println()
    println("Recommended Fixes:")
    println("1. Add occur check to unification")
    println("2. Fix induction tactic implementation")
    println("3. Improve search strategy with pruning")
    println("=" * 60)
  }
}

// ==============================================
// 期待される出力の比較
// ==============================================

/*
現在の実装の出力:
==================
[DEBUG] Unifying: n_0 with S(n_0)
[DEBUG] Unify Success: n_0 = S(n_0)  ← 問題！無限項
[DEBUG] Unifying: S(n_0) with S(S(n_0))
[DEBUG] Unifying: S(S(n_0)) with S(S(S(n_0)))
... (無限ループまたは爆発的な展開)
✗ Failed to prove (Depth 0)

改善後の出力:
==================
[DEBUG] Unifying: n_0 with S(n_0)
[DEBUG] Occur Check Failed: n_0 occurs in S(n_0)
[DEBUG] Trying alternative strategy...
[INFO] Applying induction on n
[INFO] Base case: plus(0, 0) = 0
  [DEBUG] By computation: plus(0, 0) = 0 ✓
[INFO] Inductive step: plus(S(n), 0) = S(n)
  [DEBUG] Assume IH: plus(n, 0) = n
  [DEBUG] Goal: plus(S(n), 0) = S(n)
  [DEBUG] By definition: plus(S(n), 0) = S(plus(n, 0))
  [DEBUG] By IH: S(plus(n, 0)) = S(n) ✓
✓ Proof succeeded (By induction on n)
*/
```