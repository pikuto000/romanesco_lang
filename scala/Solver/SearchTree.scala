package romanesco.Solver.core

import romanesco.Types.Tree

/**
 * 探索の結果を表す型
 */
type SearchResult = Either[FailTrace, ProofResult]

/**
 * Tree型の各ノードに格納される情報
 */
case class SearchNode(
    exprs: Vector[Expr], // このノードに至るまでの履歴
    ruleName: String,
    depth: Int,
    result: SearchResult,
    subst: Unifier.Subst,
    context: List[(String, Expr)],       // 持続的文脈
    linearContext: List[(String, Expr)]  // 線形文脈
) {
  def isSuccess: Boolean = result.isRight

  override def toString: String = {
    val mark = if (isSuccess) "✓" else "✗"
    s"[$mark] ${exprs.lastOption.getOrElse("?")} ($ruleName)"
  }

  def toJson: String = {
    def escape(s: String): String = s.replace("\"", "\\\"")
    val goalStr = exprs.lastOption.map(_.toString).getOrElse("")
    val status = if (isSuccess) "success" else "failure"
    s"""{"goal":"${escape(goalStr)}","rule":"${escape(ruleName)}","depth":$depth,"status":"$status"}"""
  }
}
