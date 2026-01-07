package romanesco

// 抽象構文木を即時評価するクラス
class Evaluator {
  
  // ノードを評価して値を返す
  def eval(node: Node): Any = {
    node.kind match {
      case "DecimalLiteral" => 
        node.attributes("value").asInstanceOf[BigDecimal]
      
      case "BinaryOp" =>
        val op = node.attributes("op").asInstanceOf[String]
        val left = eval(node.children(0))
        val right = eval(node.children(1))
        
        (left, right) match {
          case (l: BigDecimal, r: BigDecimal) =>
            op match {
              case "+" => l + r
              case "-" => l - r
              case "*" => l * r
              case "/" => l / r // BigDecimal の除算は丸めが必要になる場合があるが一旦単純に
              case ">" => l > r
              case "<" => l < r
              case ">=" => l >= r
              case "<=" => l <= r
              case _ => throw new Exception(s"[eval] Unknown operator: $op")
            }
          case (l: Boolean, r: Boolean) =>
            op match {
              case "and" => l && r
              case "or"  => l || r
              case _ => throw new Exception(s"[eval] Unknown boolean operator: $op")
            }
          case _ => 
            throw new Exception(s"[eval] Type mismatch or unsupported operands for $op: $left, $right")
        }

      case "Variable" =>
        // コンパイル時評価において変数の値が必要な場合は、
        // 外部の環境(Environment)から取得する仕組みが将来的に必要。
        // 現時点では「未定義の変数」としてエラーにするか、シンボルとして扱う。
        throw new Exception(s"[eval] Cannot evaluate variable at compile-time: ${node.tag.mangledName}")

      case _ => 
        throw new Exception(s"[eval] Node kind '${node.kind}' cannot be evaluated.")
    }
  }
}
