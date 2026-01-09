package romanesco
import scala.util.parsing.input.Position

enum CompilerError {
  case ParseError(message: String, position: Option[Position])
  case ConstraintError(message: String, node: Option[Node])
  case MacroExpansionError(message: String, macroName: String)
  case BackendError(message: String, backend: String)
  case EvaluationError(message: String, node: Option[Node])
  
  def format: String = {
    this match {
      case ParseError(msg, Some(pos)) => s"Parse error at ${pos}: $msg"
      case ParseError(msg, None) => s"Parse error: $msg"
      case ConstraintError(msg, Some(node)) => s"Constraint error at ${node.pos}: $msg"
      case ConstraintError(msg, None) => s"Constraint error: $msg"
      case MacroExpansionError(msg, name) => s"Macro expansion error in '$name': $msg"
      case BackendError(msg, backend) => s"Backend error ($backend): $msg"
      case EvaluationError(msg, Some(node)) => s"Evaluation error at ${node.pos}: $msg"
      case EvaluationError(msg, None) => s"Evaluation error: $msg"
    }
  }
}

type CompilerResult[A] = Either[List[CompilerError], A]

object CompilerResult {
  def success[A](value: A): CompilerResult[A] = Right(value)
  
  def error[A](err: CompilerError): CompilerResult[A] = Left(List(err))
  
  def errors[A](errs: List[CompilerError]): CompilerResult[A] = Left(errs)
}
