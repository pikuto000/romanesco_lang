// ==========================================
// Prover.scala
// 証明探索エンジン
// ==========================================

package romanesco.Solver.core

import scala.collection.mutable

object Prover {
  import Unifier._

  var metaCounter = 0
  def freshMeta: Expr = { metaCounter += 1; Expr.Meta(metaCounter) }

  type Context = List[(String, Expr)]

  def prove(
      goal: Expr,
      rules: List[CatRule] = StandardRules.all,
      maxDepth: Int = 30
  ): Option[Proof] =
    metaCounter = 0
    search(goal, rules, Nil, emptySubst, 0, maxDepth).map(_._1)

  private def search(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      maxDepth: Int
  ): Option[(Proof, Subst)] =
    if depth > maxDepth then None
    else
      val currentGoal = applySubst(goal, subst)

      // 探索戦略（優先度順）
      searchAxiom(currentGoal, context, subst)
        .orElse(searchReflexivity(currentGoal, subst))
        .orElse(
          searchDecompose(currentGoal, rules, context, subst, depth, maxDepth)
        )
        .orElse(
          searchRules(currentGoal, rules, context, subst, depth, maxDepth)
        )
        .orElse(
          searchContext(currentGoal, rules, context, subst, depth, maxDepth)
        )

  // 1. アクシオム検索（コンテキストに直接存在）
  private def searchAxiom(
      goal: Expr,
      context: Context,
      subst: Subst
  ): Option[(Proof, Subst)] =
    context.collectFirst { case (name, hyp) =>
      unify(hyp, goal, subst).map(s => (Nil, s))
    }.flatten

  // 2. 反射性（等式）
  private def searchReflexivity(
      goal: Expr,
      subst: Subst
  ): Option[(Proof, Subst)] =
    goal match
      case Expr.App(Expr.Sym("="), List(l, r)) => // ★修正箇所
        unify(l, r, subst).map { s =>
          (List(ProofStep.Apply(StandardRules.eqRefl, s)), s)
        }
      case _ => None

  // 3. ゴール分解（構造的導入規則）
  private def searchDecompose(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      maxDepth: Int
  ): Option[(Proof, Subst)] =
    goal match
      // 積の導入: A×B を証明 → A と B を個別に証明
      case Expr.App(Expr.Sym("×"), List(a, b)) =>
        for
          (proofA, s1) <- search(a, rules, context, subst, depth + 1, maxDepth)
          (proofB, s2) <- search(b, rules, context, s1, depth + 1, maxDepth)
        yield
          val pairStep = ProofStep.Apply(StandardRules.productUniversal, s2)
          (proofA ++ proofB :+ pairStep, s2)

      // ∧も同様
      case Expr.App(Expr.Sym("∧"), List(a, b)) =>
        searchDecompose(
          Expr.App(Expr.Sym("×"), List(a, b)),
          rules,
          context,
          subst,
          depth,
          maxDepth
        )

      // 含意の導入: A→B を証明 → A を仮定して B を証明
      case Expr.App(Expr.Sym("→"), List(a, b)) =>
        val hypName = s"h$depth"
        search(b, rules, (hypName, a) :: context, subst, depth + 1, maxDepth)
          .map { case (proof, s) =>
            val lambdaStep = ProofStep.Apply(StandardRules.expUniversal, s)
            (proof :+ lambdaStep, s)
          }

      // ^も同様
      case Expr.App(Expr.Sym("^"), List(b, a)) =>
        searchDecompose(
          Expr.App(Expr.Sym("→"), List(a, b)),
          rules,
          context,
          subst,
          depth,
          maxDepth
        )

      // 全称量化: ∀x.P を証明 → 新しい変数 x で P を証明
      case Expr.App(Expr.Sym("∀"), List(Expr.Var(v), body)) =>
        val freshVar = s"${v}_$depth"
        val instantiated = substVar(body, v, Expr.Var(freshVar))
        search(instantiated, rules, context, subst, depth + 1, maxDepth).map {
          case (proof, s) =>
            val forallStep = ProofStep.Apply(StandardRules.forallCounit, s)
            (proof :+ forallStep, s)
        }

      // 存在量化: ∃x.P を証明 → witness を見つけて P[x:=witness] を証明
      case Expr.App(Expr.Sym("∃"), List(Expr.Var(v), body)) =>
        lazy val witness = freshMeta
        lazy val instantiated = substVar(body, v, witness)
        search(instantiated, rules, context, subst, depth + 1, maxDepth).map {
          case (proof, s) =>
            val existsStep = ProofStep.Apply(StandardRules.existsUnit, s)
            (proof :+ existsStep, s)
        }

      // 余積の導入: A+B を証明 → A または B を証明
      case Expr.App(Expr.Sym("+"), List(a, b)) =>
        search(a, rules, context, subst, depth + 1, maxDepth)
          .map { case (p, s) =>
            val inlStep = ProofStep.Apply(
              CatRule("inl", a, Expr.App(Expr.Sym("+"), List(a, b))),
              s
            )
            (p :+ inlStep, s)
          }
          .orElse(
            search(b, rules, context, subst, depth + 1, maxDepth).map {
              case (p, s) =>
                val inrStep = ProofStep.Apply(
                  CatRule("inr", b, Expr.App(Expr.Sym("+"), List(a, b))),
                  s
                )
                (p :+ inrStep, s)
            }
          )

      // ∨も同様
      case Expr.App(Expr.Sym("∨"), List(a, b)) =>
        searchDecompose(
          Expr.App(Expr.Sym("+"), List(a, b)),
          rules,
          context,
          subst,
          depth,
          maxDepth
        )

      // 終対象: ⊤ または 1
      case Expr.Sym("⊤") | Expr.Sym("1") =>
        Some(
          (List(ProofStep.Apply(StandardRules.terminalUniversal, subst)), subst)
        )

      case _ => None

  // 4. 規則適用
  private def searchRules(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      maxDepth: Int
  ): Option[(Proof, Subst)] =
    rules.view.flatMap { rule =>
      unify(rule.rhs, goal, subst).flatMap { s =>
        // 普遍性条件のチェック
        val universalsOk = rule.universals.forall { cond =>
          search(cond, rules, context, s, depth + 1, maxDepth).isDefined
        }

        if universalsOk then
          // 左辺を証明
          search(rule.lhs, rules, context, s, depth + 1, maxDepth).map {
            case (proof, finalSubst) =>
              (proof :+ ProofStep.Apply(rule, finalSubst), finalSubst)
          }
        else None
      }
    }.headOption

  // 5. コンテキスト消去（矛盾、等式書き換え、分解など）
  private def searchContext(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      maxDepth: Int
  ): Option[(Proof, Subst)] =
    // 矛盾からの推論（始対象）
    context
      .collectFirst { case (name, Expr.Sym("⊥") | Expr.Sym("0")) =>
        Some(
          (List(ProofStep.Apply(StandardRules.initialUniversal, subst)), subst)
        )
      }
      .flatten
      // 等式による書き換え
      .orElse {
        context.collectFirst {
          case (name, Expr.App(Expr.Sym("="), List(l, r))) => // ★修正箇所
            val rewritten = rewriteExpr(goal, l, r)
            if rewritten != goal then
              search(rewritten, rules, context, subst, depth + 1, maxDepth)
                .map { case (proof, s) =>
                  val rewriteStep = ProofStep.Apply(StandardRules.eqSubst, s)
                  (proof :+ rewriteStep, s)
                }
            else None
        }.flatten
      }
      // コンテキストの構造分解
      .orElse(decomposeContext(goal, rules, context, subst, depth, maxDepth))

  // コンテキスト内の複雑な仮定を分解
  private def decomposeContext(
      goal: Expr,
      rules: List[CatRule],
      context: Context,
      subst: Subst,
      depth: Int,
      maxDepth: Int
  ): Option[(Proof, Subst)] =
    context.zipWithIndex.collectFirst {
      // 積の分解: A×B → A, B
      case ((name, Expr.App(Expr.Sym("×" | "∧"), List(a, b))), i) =>
        val newCtx = context.patch(
          i,
          List(
            (s"${name}.1", a),
            (s"${name}.2", b)
          ),
          1
        )
        search(goal, rules, newCtx, subst, depth + 1, maxDepth)

      // 存在量化の分解: ∃x.P → P[x:=witness]
      case ((name, Expr.App(Expr.Sym("∃"), List(Expr.Var(v), body))), i) =>
        val witness = freshMeta
        val instantiated = substVar(body, v, witness)
        val newCtx =
          context.patch(i, List((s"${name}.witness", instantiated)), 1)
        search(goal, rules, newCtx, subst, depth + 1, maxDepth)

      // 余積の分解: A+B → case分析
      case ((name, Expr.App(Expr.Sym("+" | "∨"), List(a, b))), i) =>
        // 左ケース
        val leftCtx = context.patch(i, List((s"${name}.left", a)), 1)
        search(goal, rules, leftCtx, subst, depth + 1, maxDepth)
          .orElse {
            // 右ケース
            val rightCtx = context.patch(i, List((s"${name}.right", b)), 1)
            search(goal, rules, rightCtx, subst, depth + 1, maxDepth)
          }
    }.flatten

  // 変数代入（単純版）
  private def substVar(expr: Expr, varName: String, replacement: Expr): Expr =
    expr match
      case Expr.Var(n) if n == varName => replacement
      case Expr.App(h, args)           =>
        Expr.App(
          substVar(h, varName, replacement),
          args.map(substVar(_, varName, replacement))
        )
      case _ => expr

  // 等式による書き換え
  private def rewriteExpr(expr: Expr, from: Expr, to: Expr): Expr =
    if expr == from then to
    else
      expr match
        case Expr.App(h, args) =>
          Expr.App(rewriteExpr(h, from, to), args.map(rewriteExpr(_, from, to)))
        case _ => expr
}
