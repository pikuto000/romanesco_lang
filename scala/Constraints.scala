package romanesco

import scala.collection.mutable

// ============================================================================
// 論理変数と項
// ============================================================================

/** 論理変数（参照セマンティクス） */
case class LVar(name: String){
  override def toString: String = s"?$name"
}
/** 項：変数、値、または複合項 */
enum Term[+T]:
  case Variable(v: LVar)
  case Value(v: T)
  case Composite(name: String, args: List[Term[T]])
  
  override def toString: String = this match
    case Variable(v) => v.toString
    case Value(v) => v.toString
    case Composite(n, args) => s"$n(${args.mkString(", ")})"

// ============================================================================
// 制約の定義
// ============================================================================

/** 制約の種類 */
enum Constraint[+T]{
  /** 単一化制約：left = right */
  case Unify(left: Term[T], right: Term[T])
  
  /** 述語制約：述語名と引数 */
  case Predicate(name: String, args: List[Term[T]])
  
  /** 連言制約：すべての制約を満たす */
  case Conjunction(constraints: List[Constraint[T]])
  
  /** 選言制約：いずれかの制約を満たす（非決定性） */
  case Disjunction(constraints: List[Constraint[T]])
  
  /** 否定制約：制約を満たさない */
  case Negation(c: Constraint[T])
  
  /** 常に真 */
  case True
  
  /** 常に偽 */
  case False
  
  override def toString: String = this match
    case Unify(l, r) => s"$l = $r"
    case Predicate(n, args) => s"$n(${args.mkString(", ")})"
    case Conjunction(cs) => cs.mkString(" ∧ ")
    case Disjunction(cs) => cs.mkString(" ∨ ")
    case Negation(c) => s"¬($c)"
    case True => "⊤"
    case False => "⊥"
}
// ============================================================================
// 制約ストア（単一化の状態管理）
// ============================================================================

final class ConstraintStore[T]{
  private val bindings = mutable.Map[LVar, Term[T]]()
  private val trail = mutable.Stack[(LVar, Option[Term[T]])]()
  
  /** 単一化を試行。成功したらtrue、失敗したらfalse */
  def unify(t1: Term[T], t2: Term[T]): Boolean =
    val r1 = resolve(t1)
    val r2 = resolve(t2)
    (r1, r2) match
      case (Term.Variable(v1), Term.Variable(v2)) if v1 == v2 => true
      case (Term.Variable(v), t) => bind(v, t)
      case (t, Term.Variable(v)) => bind(v, t)
      case (Term.Value(v1), Term.Value(v2)) => v1 == v2
      case (Term.Composite(n1, a1), Term.Composite(n2, a2)) =>
        n1 == n2 && a1.zip(a2).forall { case (x, y) => unify(x, y) }
      case _ => false
  
  /** 変数に項を束縛（occurs check付き） */
  private def bind(v: LVar, t: Term[T]): Boolean =
    if occursIn(v, t) then false
    else
      // トレイルに記録（バックトラッキング用）
      trail.push((v, bindings.get(v)))
      bindings(v) = t
      true
  
  /** occurs check：変数が項の中に出現するか */
  private def occursIn(v: LVar, t: Term[T]): Boolean = t match
    case Term.Variable(v2) => v == v2
    case Term.Value(_) => false
    case Term.Composite(_, args) => args.exists(occursIn(v, _))
  
  /** 項を解決（walk） */
  def resolve(t: Term[T]): Term[T] = t match
    case Term.Variable(v) =>
      bindings.get(v) match
        case Some(t2) => resolve(t2)
        case None => t
    case _ => t
  
  /** 現在の束縛状態を取得 */
  def currentBindings: Map[LVar, Term[T]] = bindings.toMap
  
  /** バックトラッキング：最後の束縛を元に戻す */
  def undo(): Unit =
    if trail.nonEmpty then
      val (v, oldValue) = trail.pop()
      oldValue match
        case Some(t) => bindings(v) = t
        case None => bindings.remove(v)
  
  /** バックトラッキング：n回元に戻す */
  def undo(n: Int): Unit = (1 to n).foreach(_ => undo())
  
  /** トレイルの深さを取得 */
  def trailDepth: Int = trail.size
  
  /** 特定の深さまで戻す */
  def unwindTo(depth: Int): Unit =
    while trail.size > depth do undo()
  
  /** 制約を適用 */
  def applyConstraint(c: Constraint[T]): Boolean = c match
    case Constraint.Unify(l, r) => unify(l, r)
    case Constraint.Conjunction(cs) => cs.forall(applyConstraint)
    case Constraint.Disjunction(cs) => 
      // 選言は非決定的に扱う必要があるため、ここでは単純化
      cs.exists(applyConstraint)
    case Constraint.Negation(c) => !applyConstraint(c)
    case Constraint.True => true
    case Constraint.False => false
    case Constraint.Predicate(name, args) =>
      // ユーザー定義述語は外部で処理
      // ここでは単純にtrueを返す（拡張ポイント）
      true
  
  override def toString: String =
    bindings.map { case (k, v) => s"$k = $v" }.mkString(", ")
}
// ============================================================================
// 制約付きTree（非決定性 + 制約）
// ============================================================================

/** 制約付きTree：非決定的な可能性に制約を付与 */
enum ConstrainedTree[T, C]{
  /** 確定したTree（すべての制約が充足） */
  case Resolved(tree: Tree[T], store: ConstraintStore[C])
  
  /** 未解決の可能性（制約適用前または適用中） */
  case Unresolved(alternatives: Vector[Tree[T]], constraints: Constraint[C])
  
  /** 制約充足不能（失敗） */
  case Failed()
  
  /** 非決定的な選択（選言） */
  case Choice(branches: Vector[ConstrainedTree[T, C]])
  
  /** 制約を適用して解決を試みる */
  def resolve(store: ConstraintStore[C]): ConstrainedTree[T, C] = this match
    case Unresolved(alts, cons) =>
      val results = alts.flatMap { tree =>
        val depth = store.trailDepth
        val success = store.applyConstraint(cons)
        if success then
          Some(Resolved(tree, store))
        else
          store.unwindTo(depth)
          None
      }
      if results.isEmpty then Failed()
      else if results.size == 1 then results.head
      else Choice(results)
      
    case Choice(branches) =>
      val resolved = branches.map(_.resolve(store)).filter(_ != Failed)
      if resolved.isEmpty then Failed()
      else if resolved.size == 1 then resolved.head
      else Choice(resolved)
      
    case _ => this
  
  /** すべての解を収集 */
  def collect: Vector[(Tree[T], ConstraintStore[C])] = this match
    case Resolved(t, s) => Vector((t, s))
    case Choice(bs) => bs.flatMap(_.collect)
    case _ => Vector.empty
}
// ============================================================================
// 制約ソルバー（高度な制約解消）
// ============================================================================

class ConstraintSolver[T]{
  private val store = ConstraintStore[T]()
  
  /** 制約セットを解消 */
  def solve(constraint: Constraint[T]): Option[ConstraintStore[T]] =
    val depth = store.trailDepth
    val success = solveRecursive(constraint)
    if success then Some(store) else
      store.unwindTo(depth)
      None
  
  private def solveRecursive(c: Constraint[T]): Boolean = c match
    case Constraint.Unify(l, r) => store.unify(l, r)
    case Constraint.Conjunction(cs) => cs.forall(solveRecursive)
    case Constraint.Disjunction(cs) => 
      // 非決定的：最初に成功したものを選ぶ（バックトラッキング可能）
      cs.exists { c =>
        val depth = store.trailDepth
        val result = solveRecursive(c)
        if !result then store.unwindTo(depth)
        result
      }
    case Constraint.Negation(c) => !solveRecursive(c)
    case Constraint.True => true
    case Constraint.False => false
    case Constraint.Predicate(name, args) =>
      // 組み込み述語の処理
      name match
        case "integer" => args.headOption.exists(a => checkInteger(store.resolve(a)))
        case "string" => args.headOption.exists(a => checkString(store.resolve(a)))
        case "greater_than" => 
          args match
            case List(a, b) => compareValues(store.resolve(a), store.resolve(b))(_ > _)
            case _ => false
        case _ => false
  
  private def checkInteger(t: Term[T]): Boolean = t match
    case Term.Value(v: Int) => true
    case _ => false
  
  private def checkString(t: Term[T]): Boolean = t match
    case Term.Value(v: String) => true
    case _ => false
  
  private def compareValues(a: Term[T], b: Term[T])(op: (Int, Int) => Boolean): Boolean =
    (a, b) match
      case (Term.Value(v1: Int), Term.Value(v2: Int)) => op(v1, v2)
      case _ => false
}
// ============================================================================
// Treeとの統合（制約付きパース）
// ============================================================================

object ConstrainedParsing{
  /** TokenTreeに制約を適用して絞り込み */
  def filterByConstraint[T](
    tree: Tree[T], 
    constraint: T => Boolean
  ): ConstrainedTree[T, Nothing] =
    val alternatives = tree match
      case Tree.E() => Vector.empty
      case Tree.V(v, branches) =>
        if constraint(v) then
          val filteredBranches = branches.map(filterByConstraint(_, constraint))
          // 簡略化：ここでは単純に通過
          Vector(Tree.V(v, branches))
        else Vector.empty
    
    if alternatives.isEmpty then ConstrainedTree.Failed()
    else if alternatives.size == 1 then 
      ConstrainedTree.Resolved(alternatives.head, ConstraintStore())
    else 
      ConstrainedTree.Unresolved(alternatives, Constraint.True)
  
  /** 二つのTreeの単一化を試行 */
  def unifyTrees[T](t1: Tree[Term[T]], t2: Tree[Term[T]]): ConstrainedTree[Term[T], T] =
    val store = ConstraintStore[T]()
    val success = unifyTreeRecursive(t1, t2, store)
    if success then
      ConstrainedTree.Resolved(t1, store)
    else
      ConstrainedTree.Failed()
  
  private def unifyTreeRecursive[T](
    t1: Tree[Term[T]], 
    t2: Tree[Term[T]], 
    store: ConstraintStore[T]
  ): Boolean = (t1, t2) match
    case (Tree.E(), Tree.E()) => true
    case (Tree.V(v1, b1), Tree.V(v2, b2)) =>
      store.unify(v1, v2) && b1.zip(b2).forall { case (x, y) => unifyTreeRecursive(x, y, store) }
    case _ => false
}