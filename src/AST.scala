package romanesco
import scala.util.parsing.input.Positional

// 汎用ASTノード
case class Node(
  kind: String, // "Variable", "IntLiteral", "Unification", "BinaryOp" 等
  tag: HygenicTag,
  attributes: Map[String, Any] = Map.empty,
  children: List[Node] = Nil
) extends Positional {
  override def toString: String = {
    val attrs = if (attributes.nonEmpty) attributes.toString else ""
    val kids = if (children.nonEmpty) children.mkString("(", ", ", ")") else ""
    s"$kind$attrs$kids"
  }
}