package romanesco
import scala.util.matching.Regex
import Predicates.Predicate
import Predicates.Token

trait ParseRule[T, R] {
  def name: String
  // Returns Vector of (ResultNode, RemainingBranches)
  // RemainingBranches is Vector[Tree[Token]] (the next nodes to process)
  def apply(cursor: Tree[Token]): Vector[(Tree[R], Vector[Tree[Token]])]
}

class StandardRule[T, R](
  val name: String,
  val pattern: Vector[Predicate],
  val build: Vector[T] => Tree[R]
) extends ParseRule[T, R] {

  override def apply(cursor: Tree[Token]): Vector[(Tree[R], Vector[Tree[Token]])] = {
    // Start matching from the children of the cursor
    val startNodes = cursor match {
      case Tree.V(_, branches) => branches
      case Tree.E() => Vector.empty
    }

    matchSequence(pattern.toList, startNodes, Vector.empty)
  }

  private def matchSequence(
    preds: List[Predicate],
    candidates: Vector[Tree[Token]],
    accum: Vector[Token]
  ): Vector[(Tree[R], Vector[Tree[Token]])] = {
    preds match {
      case Nil =>
        // Pattern matched completely
        // Cast Token to T (Assuming T handles Token or the user knows what they are doing)
        val inputs = accum.map(_.asInstanceOf[T])
        Vector((build(inputs), candidates))
      
      case p :: rest =>
        // Try to find a match among candidates
        candidates.flatMap { node =>
          node match {
            case Tree.V(token, nextBranches) =>
              if (p.check(token)) {
                matchSequence(rest, nextBranches, accum :+ token)
              } else {
                Vector.empty
              }
            case Tree.E() => Vector.empty
          }
        }
    }
  }
}
