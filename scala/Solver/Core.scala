// ==========================================
// Core.scala
// 基本的なデータ構造の定義（Appによるラムダ表現版）
// ==========================================

package romanesco.Solver.core

/** メタ変数のユニークな識別子
  */
case class MetaId(ids: List[Int]) {
  override def toString: String = ids.mkString(".")
}

object MetaId {
  def apply(id: Int): MetaId = MetaId(List(id))
}

/** 論理式を表現する抽象構文木
  */
enum Expr:
  case Var(name: String) // 束縛変数 / 個体変数
  case Meta(id: MetaId) // メタ変数
  case Sym(name: String) // 定数
  case App(f: Expr, args: List[Expr])

  def apply(args: Expr*): Expr = App(this, args.toList)

  /** 目標の先頭記号（インデックス用）を返します */
  def headSymbol: String = this match
    case App(Sym(n), _) => n
    case Sym(n)         => n
    case Var(_)         => "_VAR_"
    case Meta(_)        => "_META_"
    case App(h, _)      => h.headSymbol

  override def toString: String = this match
    case Var(n)                                              => n
    case Meta(id)                                            => s"?$id"
    case Sym(n)                                              => n
    case App(Expr.Sym("Type"), List(Expr.Meta(MetaId(ids)))) =>
      s"Type${ids.length}"
    case App(Expr.Sym("path"), args) =>
      def getPathLevel(e: Expr): Int = e match {
        case App(Expr.Sym("path"), List(sub, _, _)) => 1 + getPathLevel(sub)
        case _                                      => 0
      }
      val level = getPathLevel(this)
      if (level > 1) {
        val superscripts = "⁰¹²³⁴⁵⁶⁷⁸⁹"
        val levelStr = level.toString
          .map(c => if (c >= '0' && c <= '9') superscripts(c - '0') else c)
          .mkString
        s"path$levelStr(${args.mkString(",")})"
      } else {
        s"path(${args.mkString(",")})"
      }
    case App(Expr.Sym("λ"), List(Expr.Var(v), b)) => s"λ$v. $b"
    case App(Expr.Sym("="), List(l, r))           => s"$l = $r"
    case App(h, args)                             =>
      if args.isEmpty then h.toString
      else s"$h(${args.mkString(",")})"

  /** メタ変数と変数を出現順に正規化（α同値性の判定用） */
  def canonicalize(renameMeta: Boolean = true): Expr = {
    val metaMap = scala.collection.mutable.Map[MetaId, MetaId]()
    val varMap = scala.collection.mutable.Map[String, String]()
    var metaCounter = 0
    var varCounter = 0
    
    // De Bruijn Index ベースの完全な実装
    def deBruijn(e: Expr, env: List[String]): Expr = e match {
      case Meta(id) =>
        if (renameMeta) {
          val newId = metaMap.getOrElseUpdate(id, { val n = MetaId(List(metaCounter)); metaCounter += 1; n })
          Meta(newId)
        } else Meta(id)
      case Var(name) =>
        val idx = env.indexOf(name)
        if (idx >= 0) Var(s"bv$idx") // Bound variable (index)
        else Var(name) // Free variable
      case App(Sym("λ"), List(Var(v), body)) =>
        App(Sym("λ"), List(Var(s"bv0"), deBruijn(body, v :: env))) 
      case App(f, args) => App(deBruijn(f, env), args.map(deBruijn(_, env)))
      case _ => e
    }
    
    deBruijn(this, Nil)
  }

  /** 式の複雑さ（ヒューリスティック用スコア） */
  def complexity: Int = this match {
    case Var(_) | Meta(_) | Sym(_) => 1
    case App(f, as) => 
      val maxDepth = if (as.isEmpty) 0 else as.map(_.complexity).max
      f.complexity + maxDepth + 1
  }

  def contains(other: Expr): Boolean = {
    if (this == other) true
    else this match {
      case App(f, args) => f.contains(other) || args.exists(_.contains(other))
      case _ => false
    }
  }

  /** 構造的パターンの抽象化（循環検知用） */
  def getStructuralPattern: String = {
    var varCounter = 0
    val varMap = scala.collection.mutable.Map[String, String]()
    def loop(e: Expr): String = e match {
      case Var(name) =>
        val n = varMap.getOrElseUpdate(name, {
          val res = s"V$varCounter"
          varCounter += 1
          res
        })
        n
      case Sym(n)  => s"S($n)"
      case Meta(_) => "M"
      case App(f, args) =>
        s"A(${loop(f)},${args.map(loop).mkString(",")})"
    }
    loop(this)
  }

  def toJson: String = {
    def escape(s: String): String = s.replace("\"", "\\\"")
    this match {
      case Var(n) => s"{\"type\":\"Var\",\"name\":\"${escape(n)}\"}"
      case Sym(n) => s"{\"type\":\"Sym\",\"name\":\"${escape(n)}\"}"
      case Meta(id) => s"{\"type\":\"Meta\",\"id\":\"$id\"}"
      case App(f, args) => s"{\"type\":\"App\",\"f\":${f.toJson},\"args\":[${args.map(_.toJson).mkString(",")}]}"
    }
  }

object Expr:
  def sym(name: String): Expr = Sym(name)
  def v(name: String): Expr = Var(name)
  def meta(id: Int): Expr = Meta(MetaId(id))
  def meta(ids: Int*): Expr = Meta(MetaId(ids.toList))

  def typeLevel(level: Int): Expr =
    App(Sym("Type"), List(Meta(MetaId((0 until level).toList.map(_ => 0)))))

  def unapplyEq(e: Expr): Option[(Expr, Expr)] = e match
    case App(Sym("="), List(l, r)) => Some((l, r))
    case _                         => None

  // ラムダ式の抽出用パターン
  object Lam:
    def unapply(e: Expr): Option[(String, Expr)] = e match
      case App(Sym("λ"), List(Var(v), body)) => Some((v, body))
      case _                                 => None

/** 圏論得推論規則
  */
case class CatRule(
    name: String,
    lhs: Expr,
    rhs: Expr,
    universals: List[Expr] = Nil,
    priority: Int = 10,
    domain: String = "general"
):
  override def toString: String =
    val cond =
      if universals.isEmpty then ""
      else s" where ${universals.mkString(", ")}"
    s"[$domain] $name: $lhs ⟹ $rhs$cond (prio: $priority)"

  def toJson: String = {
    def escape(s: String): String = s.replace("\"", "\\\"")
    s"{\"name\":\"${escape(name)}\",\"lhs\":${lhs.toJson},\"rhs\":${rhs.toJson},\"universals\":[${universals.map(_.toJson).mkString(",")}],\"priority\":$priority,\"domain\":\"${escape(domain)}\"}"
  }

/** 証明のツリー構造
  */
enum ProofTree:
  case Node(goal: Expr, ruleName: String, children: List[ProofTree])
  case Leaf(goal: Expr, ruleName: String)

  def format(indent: Int = 0): String =
    val sp = "  " * indent
    this match
      case Leaf(g, r)     => s"$sp✓ $g  {$r}"
      case Node(g, r, cs) =>
        val childrenStr = cs.map(_.format(indent + 1)).mkString("\n")
        s"$childrenStr\n$sp└─ $g  {$r}"

  def toJson: String = this match {
    case Leaf(g, r) => s"{\"nodeType\":\"Leaf\",\"goal\":\"$g\",\"rule\":\"$r\"}"
    case Node(g, r, cs) => s"{\"nodeType\":\"Node\",\"goal\":\"$g\",\"rule\":\"$r\",\"children\":[${cs.map(_.toJson).mkString(",")}]}"
  }

type Proof = ProofTree

/** 証明の失敗トレース
  */
case class FailTrace(
    goal: Goal,
    reason: String,
    depth: Int,
    children: List[FailTrace] = Nil,
    failureType: String = "Normal", // "Unify", "Depth", "Cycle", etc.
    attemptedRules: List[String] = Nil,
    unificationFailures: List[(Expr, Expr)] = Nil
):
  def format(indent: Int = 0): String =
    val sp = "  " * indent
    val typeMark = if failureType == "Normal" then "✗" else s"[$failureType]"
    var base =
      s"$sp$typeMark (Depth $depth) Goal: ${goal.target}\n$sp  Reason: $reason"
    
    if (attemptedRules.nonEmpty) {
      base += s"\n$sp  Attempted Rules: ${attemptedRules.mkString(", ")}"
    }
    if (unificationFailures.nonEmpty) {
      base += s"\n$sp  Unification Failures:"
      unificationFailures.foreach { case (e1, e2) =>
        base += s"\n$sp    $e1 != $e2"
      }
    }

    if children.isEmpty then base
    else s"$base\n${children.map(_.format(indent + 1)).mkString("\n")}"

import romanesco.Types.Tree

/** 証明結果
  */
case class ProofResult(
    tree: ProofTree,
    generatedLemma: Option[CatRule] = None,
    searchTree: Option[Tree[SearchNode]] = None
)

/** コンストラクタの引数定義
  */
enum ArgType:
  case Recursive
  case Constant

/** コンストラクタの型（点または道）
  */
enum ConstructorType:
  case Point
  case Path(from: Expr, to: Expr)

/** 代数の構造定義 */
case class ConstructorDef(
    symbol: String,
    argTypes: List[ArgType],
    ctorType: ConstructorType = ConstructorType.Point
)
case class InitialAlgebra(
    name: String,
    constructors: List[ConstructorDef],
    varPrefix: String
)

/** Lemma生成の制御モード
  */
enum LemmaGenerationMode:
  case All // 全ての成功したゴールから生成
  case InductionOnly // 帰納法が成功した時のみ生成
  case EqualityOnly // 等式のみ生成
  case ManualOnly // ユーザーが明示的にフラグを立てた時のみ（未実装）

/** 証明エンジンの設定
  */
case class ProverConfig(
    classical: Boolean = false,
    rules: List[CatRule] = Nil,
    algebras: List[InitialAlgebra] = Nil,
    maxRaa: Int = 2,
    maxInduction: Int = 2,
    maxPathLevel: Int = 5,
    maxComplexity: Int = 200,
    maxParallelism: Int = 8,
    generateLemmas: Boolean = true,
    lemmaMode: LemmaGenerationMode = LemmaGenerationMode.EqualityOnly,
    excludeTrivialLemmas: Boolean = true,
    enabledDomains: Option[Set[String]] = None
)

object ProverConfig {
  def default: ProverConfig = ProverConfig()
}

// --- タクティクスシステム用の定義 ---

case class Goal(
    context: List[(String, Expr)],
    linearContext: List[(String, Expr)] = Nil,
    target: Expr
):
  override def toString: String =
    val ctx = context.map((n, e) => s"  $n: $e").mkString("\n")
    val lctx =
      if (linearContext.isEmpty) ""
      else
        "\n  (Linear):\n" + linearContext
          .map((n, e) => s"  $n: $e")
          .mkString("\n")
    s"--------------------------------\n$ctx$lctx\n--------------------------------\n  Goal: $target"

case class ProofState(
    goals: List[Goal],
    completedProofs: List[Proof],
    originalGoal: Expr,
    searchTree: Option[Tree[SearchNode]] = None,
    rules: List[CatRule] = Nil
):
  def isSolved: Boolean = goals.isEmpty
  def currentGoal: Option[Goal] = goals.headOption

type TacticResult = Either[String, ProofState]
