package romanesco

import scala.util.matching.Regex

// ============================================================================
// Tokenizerとの統合：トークンに制約を適用
// ============================================================================

object TokenConstraint{
  /** トークン型のTerm表現 */
  type TokenTerm = Term[(Int, Int, Regex, String)]
  
  /** トークンからTermを生成 */
  def tokenToTerm(t: (Int, Int, Regex, String)): TokenTerm =Term.Value(t)
  
  /** 内容による制約 */
  def contentIs(expected: String): Constraint[(Int, Int, Regex, String)] = Constraint.Predicate("content_is", List(Term.Value((0, 0, "".r, expected))))
  /** トークン型による制約 */
  def hasType(typeName: String): Constraint[(Int, Int, Regex, String)] =
    Constraint.Predicate("has_type", List(Term.Value((0,0,typeName.r,""))))
  
  /** TokenTreeを制約でフィルタリング */
  def filterTree(
    tree: Tree[(Int, Int, Regex, String)],
    pred: ((Int, Int, Regex, String)) => Boolean
  ): ConstrainedTree[(Int, Int, Regex, String), (Int, Int, Regex, String)] =
    tree match
      case Tree.E() => ConstrainedTree.Failed()
      case Tree.V(token, branches) =>
        if pred(token) then
          val store = ConstraintStore[(Int, Int, Regex, String)]()
          ConstrainedTree.Resolved(Tree.V(token, branches), store)
        else
          ConstrainedTree.Failed()
}
// ============================================================================
// Parserとの統合：パース結果に制約を適用
// ============================================================================

class ConstrainedParser(rules: Map[String, ParseRule]):
  type Token = Tokenizer#Token
  type TokenTree = Tree[Token]
  type ParseResult = (String, Vector[String])
  type ParseTree = Tree[ParseResult]
  
  private val baseParser = rParser(rules)
  
  /** 制約付きパース：パース結果に制約を適用 */
  def parseWithConstraints(
    tokenTree: TokenTree,
    constraint: Constraint[ParseResult]
  ): ConstrainedTree[ParseResult, ParseResult] =
    val rawResults = baseParser.parse(tokenTree)
    
    // 各結果に制約を適用
    val constrained = rawResults match
      case Tree.V(("ParseRoot", _), branches) =>
        branches.flatMap { branch =>
          applyConstraintToTree(branch, constraint)
        }
      case _ => Vector.empty
    
    if constrained.isEmpty then ConstrainedTree.Failed()
    else if constrained.size == 1 then constrained.head
    else ConstrainedTree.Choice(constrained)
  
  private def applyConstraintToTree(
    tree: ParseTree,
    constraint: Constraint[ParseResult]
  ): Vector[ConstrainedTree[ParseResult, ParseResult]] =
    val store = ConstraintStore[ParseResult]()
    val term = parseResultToTerm(tree)
    
    // 制約をTermレベルで適用
    val unified = constraint match
      case Constraint.Unify(left, right) =>
        store.unify(term, left) || store.unify(term, right)
      case Constraint.Predicate(name, args) =>
        // 述語制約の評価
        evaluatePredicate(name, tree, args)
      case Constraint.True => true
      case _ => true  // 簡略化
    
    if unified then
      Vector(ConstrainedTree.Resolved(tree, store))
    else
      Vector.empty
  
  private def parseResultToTerm(tree: ParseTree): Term[ParseResult] = tree match
    case Tree.E() => Term.Composite("empty", Nil)
    case Tree.V((op, operands), branches) =>
      Term.Composite("node", List(
        Term.Value((op, operands)),
        Term.Composite("operands", operands.map(v => Term.Value((v, Vector.empty))).toList),
        Term.Composite("children", branches.map(parseResultToTerm).toList)
      ))
  
  private def evaluatePredicate(
    name: String, 
    tree: ParseTree, 
    args: List[Term[ParseResult]]
  ): Boolean = name match
    case "has_operator" =>
      tree match
        case Tree.V((op, _), _) => 
          args.headOption.exists:
            case Term.Value((expected: String, _)) => op == expected
            case _ => false
        case _ => false
    case "depth_less_than" =>
      args.headOption.exists:
        case Term.Value((n: String, _)) => treeDepth(tree) < n.toInt
        case _ => false
    case _ => true
  
  private def treeDepth(tree: ParseTree): Int = tree match
    case Tree.E() => 0
    case Tree.V(_, branches) => 
      1 + branches.map(treeDepth).maxOption.getOrElse(0)

// ============================================================================
// 使用例：制約付き字句解析と構文解析
// ============================================================================

object ConstraintExamples:
  /** 例1：特定のパターンを持つトークンのみを抽出 */
  def example1(): Unit =
    val rules = Map(
      "identifier" -> "[a-zA-Z_][a-zA-Z0-9_]*".r,
      "number" -> "[0-9]+".r,
      "space" -> "\\s+".r
    )
    
    val tokenizer = Tokenizer(rules)
    val tree = tokenizer.toknize("abc 123 def")
    
    // identifierのみを抽出する制約
    val constraint = Constraint.Predicate[(Int, Int, Regex, String)](
      "is_identifier",
      List(Term.Variable(LVar("x")))
    )
    
    println("Example 1: Token filtering with constraints")
    println(tree.prettyPrint())
  
  /** 例2：パース結果の構造に制約を適用 */
  def example2(): Unit =
    // パース規則の定義
    val parseRules = Map(
      "Expr" -> StandardRule(
        name = "Expr",
        pattern = Vector(
          Predicates.matches { case (_, _, _, content) => content.forall(_.isDigit) }
        ),
        build = children => ("Number", children.map(_._4))
      )
    )
    
    val parser = ConstrainedParser(parseRules)
    
    // 制約：演算子が"Number"であること
    val constraint = Constraint.Predicate[((String, Vector[String]))](
      "has_operator",
      List(Term.Value("Number",Vector.empty))
    )
    
    println("Example 2: Parse constraint")
  
  /** 例3：単一化によるパターンマッチ */
  def example3(): Unit =
    val store = ConstraintStore[String]()
    
    // パターン: node("Add", [x, y])
    val pattern = Term.Composite[String]("node", List(
      Term.Value("Add"),
      Term.Composite("args", List(
        Term.Variable(LVar("x")),
        Term.Variable(LVar("y"))
      ))
    ))
    
    // 実際の値: node("Add", ["1", "2"])
    val value = Term.Composite[String]("node", List(
      Term.Value("Add"),
      Term.Composite("args", List(
        Term.Value("1"),
        Term.Value("2")
      ))
    ))
    
    val success = store.unify(pattern, value)
    println(s"Example 3: Unification result = $success")
    println(s"Bindings: ${store.currentBindings}")
    // 結果: x = "1", y = "2"

// ============================================================================
// 高度な制約：オフサイドルールの実装例
// ============================================================================

object OffsideRule:
  /** インデント情報を持つトークン */
  type IndentToken = (Int, Int, Regex, String, Int)  // row, col, regex, content, indent
  
  /** オフサイドルール制約：現在のインデントより深いブロック */
  def offsideConstraint(currentIndent: Int): Constraint[IndentToken] =
    Constraint.Predicate[IndentToken](
      "indent_greater_than",
      List(Term.Value((0,0,"".r, "",currentIndent)))
    )
  
  /** インデントレベルの検証 */
  def validateIndent(tree: Tree[IndentToken], baseIndent: Int): Boolean =
    tree match
      case Tree.E() => true
      case Tree.V((row, col, _, _, indent), branches) =>
        indent >= baseIndent && branches.forall(validateIndent(_, baseIndent))

// ============================================================================
// 制約による曖昧性解消（非決定性Treeの絞り込み）
// ============================================================================

object AmbiguityResolution:
  /** 複数のパース結果から制約で最良のものを選択 */
  def selectBest[T](
    alternatives: Vector[Tree[T]],
    scoring: Tree[T] => Int
  ): Option[Tree[T]] =
    if alternatives.isEmpty then None
    else
      // スコアが最も高いものを選択
      Some(alternatives.maxBy(scoring))
  
  /** 型制約による絞り込み（カリー＝ハワード対応） */
  def filterByType[T](
    trees: Vector[Tree[Term[T]]],
    expectedType: Term[T]
  ): ConstrainedTree[Term[T], T] =
    val store = ConstraintStore[T]()
    val results = trees.filter { tree =>
      val depth = store.trailDepth
      val term = tree match
        case Tree.V(t, _) => t
        case Tree.E() => Term.Composite("empty", Nil)
      
      val success = store.unify(term, expectedType)
      if !success then store.unwindTo(depth)
      success
    }
    
    if results.isEmpty then ConstrainedTree.Failed()
    else if results.size == 1 then 
      ConstrainedTree.Resolved(results.head, store)
    else 
      ConstrainedTree.Unresolved(results, Constraint.Unify(
        Term.Variable(LVar("result")), 
        expectedType
      ))
