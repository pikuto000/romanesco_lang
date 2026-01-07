package romanesco

case class Node(
  kind: String,
  tag: HygenicTag,
  attributes: Map[String, Any] = Map.empty,
  children: List[Node] = Nil
) {
  // タグの情報から論理的不透明性を判断
  def isOpaque: Boolean = tag.isOpaque

  override def toString: String = {
    val attrStr = if (attributes.isEmpty) "" else "Map(" + attributes.map { case (k, v) => s"$k -> $v" }.mkString(", ") + ")"
    val childStr = if (children.isEmpty) "" else "(" + children.map(_.toString).mkString(", ") + ")"
    val opaqueMark = if (isOpaque) "[O]" else ""
    s"$kind$opaqueMark:${tag.mangledName}$attrStr$childStr"
  }

  def copy(
    kind: String = this.kind,
    tag: HygenicTag = this.tag,
    attributes: Map[String, Any] = this.attributes,
    children: List[Node] = this.children
  ): Node = Node(kind, tag, attributes, children)
}