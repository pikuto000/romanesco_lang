package romanesco

object ResultPrinter {
  def printResults(nodes: Array[Any]): Unit = {
    println("--- Computed Results ---")
    nodes.foreach {
      case node: Node => printNode(node, "")
      case other => println(s"Unknown result: $other")
    }
    println("------------------------")
  }
  
  private def printNode(node: Node, indent: String): Unit = {
    node.kind match {
      case "Program" => {
        node.children.foreach(child => printNode(child, indent))
      }
      
      case "DecimalLiteral" => {
        val value = node.requireNum("value")
        println(s"${indent}result = $value")
      }
      
      case "BinaryOp" => {
        val op = node.requireOp("op")
        print(s"${indent}(")
        printNode(node.children(0), "")
        print(s" $op ")
        printNode(node.children(1), "")
        print(")")
      }
      
      case "Variable" => {
        print(node.tag.name.split(":").lastOption.getOrElse(node.tag.mangledName))
      }
      
      case _ => {
        println(s"${indent}${node.kind}")
      }
    }
  }
}
