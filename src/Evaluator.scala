package romanesco

class Evaluator {
  
  def eval(node: Node): Either[CompilerError, Any] = {
    node.kind match {
      case "DecimalLiteral" => {
        node.getNum("value") match {
          case Some(value) => Right(value)
          case None => Left(CompilerError.EvaluationError(
            "Missing value in decimal literal", 
            Some(node)
          ))
        }
      }
      
      case "BinaryOp" => {
        for {
          op <- node.getOp("op").toRight(
            CompilerError.EvaluationError("Missing operator", Some(node))
          )
          left <- eval(node.children(0))
          right <- eval(node.children(1))
          result <- evalBinaryOp(op, left, right, node)
        } yield result
      }
      
      case "Variable" => {
        Left(CompilerError.EvaluationError(
          s"Cannot evaluate variable at compile-time: ${node.tag.mangledName}",
          Some(node)
        ))
      }
      
      case _ => {
        Left(CompilerError.EvaluationError(
          s"Node kind '${node.kind}' cannot be evaluated",
          Some(node)
        ))
      }
    }
  }
  
  private def evalBinaryOp(
    op: String, 
    left: Any, 
    right: Any,
    node: Node
  ): Either[CompilerError, Any] = {
    (left, right) match {
      case (l: BigDecimal, r: BigDecimal) => {
        op match {
          case "+" => Right(l + r)
          case "-" => Right(l - r)
          case "*" => Right(l * r)
          case "/" => {
            if (r != 0) {
              Right(l / r)
            } else {
              Left(CompilerError.ConstraintError("Division by zero", Some(node)))
            }
          }
          case ">" => Right(l > r)
          case "<" => Right(l < r)
          case ">=" => Right(l >= r)
          case "<=" => Right(l <= r)
          case _ => Left(CompilerError.EvaluationError(
            s"Unknown arithmetic operator: $op",
            Some(node)
          ))
        }
      }
      
      case (l: Boolean, r: Boolean) => {
        op match {
          case "and" => Right(l && r)
          case "or" => Right(l || r)
          case _ => Left(CompilerError.EvaluationError(
            s"Unknown boolean operator: $op",
            Some(node)
          ))
        }
      }
      
      case _ => {
        Left(CompilerError.EvaluationError(
          s"Type mismatch for operator $op: $left, $right",
          Some(node)
        ))
      }
    }
  }
}
