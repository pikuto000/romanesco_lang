package romanesco.Parser
import scala.util.matching.Regex
import Predicates.Predicate
import Predicates.Token
import romanesco.Parser.Predicates.TokenTree
import romanesco.Types._

trait ParseRule {
  def name: String
  // Returns Vector of (ResultNode, RemainingBranches)
  // RemainingBranches is Vector[Tree[Token]] (the next nodes to process)
  def apply(
      cursor: TokenTree
  ): Vector[(Tuple2[String, Vector[String]], Vector[TokenTree])]
}

final class StandardRule(
    val name: String, // Name of the rule for debugging purposes
    val pattern: Vector[Predicate], // List of predicates to match
    val build: Vector[Token] => Tuple2[String, Vector[
      String
    ]] // Function to build the result node
) extends ParseRule {
  override def apply(
      cursor: Tree[Token]
  ): Vector[(Tuple2[String, Vector[String]], Vector[TokenTree])] = {
    // Start matching from the children of the cursor
    val startNodes: Vector[TokenTree] = cursor match {
      case Tree.V(_, branches: Vector[TokenTree]) => branches
      case Tree.E()                               => Vector.empty
    }

    matchSequence(pattern.toList, startNodes, Vector.empty)
  }

  private def matchSequence(
      preds: List[Predicate], // List of predicates to match
      candidates: Vector[TokenTree], // Vector of possible next nodes
      accum: Vector[Token] // Vector of tokens matched so far
  ): Vector[(Tuple2[String, Vector[String]], Vector[TokenTree])] = {
    preds match {
      case Nil =>
        // Pattern matched completely
        // Cast Token to T (Assuming T handles Token or the user knows what they are doing)
        val inputs = accum
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
