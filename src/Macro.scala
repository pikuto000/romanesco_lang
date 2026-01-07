package romanesco

object Macro {
  
  object ConstantFolding {
    private val evaluator = new Evaluator()

    // AST全体（ノードの配列）に対して定数畳み込みを適用
    def apply(nodes: Array[Any]): Array[Any] = {
      nodes.map {
        case node: Node => fold(node)
        case other => other
      }
    }

    // 個別のノードを再帰的に畳み込む
    private def fold(node: Node): Node = {
      // まず子ノードを再帰的に畳み込む
      val foldedChildren = node.children.map(fold)
      val foldedNode = node.copy(children = foldedChildren)

      foldedNode.kind match {
        case "BinaryOp" =>
          // 両辺が DecimalLiteral なら計算して置換
          if (foldedChildren.length == 2) {
            val left = foldedChildren(0)
            val right = foldedChildren(1)
            
            if (left.kind == "DecimalLiteral" && right.kind == "DecimalLiteral") {
              try {
                val result = evaluator.eval(foldedNode)
                result match {
                  case d: BigDecimal =>
                    logger.log(s"[macro] Folded constant expression: $foldedNode -> $d")
                    Node("DecimalLiteral", node.tag, Map("value" -> d))
                  case b: Boolean =>
                    // 比較演算の結果（Boolean）は現状ASTで表現できないが、
                    // BoolLiteralを作るならここで対応可能
                    foldedNode
                  case _ => foldedNode
                }
              } catch {
                case e: Exception => 
                  logger.log(s"[macro] Failed to fold $foldedNode: ${e.getMessage}")
                  foldedNode
              }
            } else {
              foldedNode
            }
          } else {
            foldedNode
          }
        
        case "Unification" =>
          // Unification 自体は畳み込まないが、子ノード（右辺など）は既に畳み込み済み
          foldedNode

        case _ => foldedNode
      }
    }
  }
}