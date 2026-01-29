package romanesco
import scala.collection.immutable
import Predicates.Token
import Debug.logger

final class rParser[T](rules: immutable.Map[String, ParseRule[Any, Any]]) {

  def parse(tokenTree: Tree[Token]): Tree[Any] = {
    logger.log("Starting parsing...")
    // Start parsing. tokenTree root is SOF.
    val results = parseRecursive(tokenTree)
    
    // Merge results into a single tree structure
    val merged = Tree.merge(results)
    
    // Wrap in a Root node
    Tree.V("ParseRoot", merged)
  }

  private def parseRecursive(cursor: Tree[Token]): Vector[Tree[Any]] = {
    // 1. Try to match rules at the current position (looking at children of cursor)
    val ruleMatches = rules.values.toVector.flatMap { rule =>
      rule.apply(cursor)
    }

    val ruleResults = if (ruleMatches.nonEmpty) {
      ruleMatches.flatMap { case (node, nextBranches) =>
        // Continue parsing from the branches where the rule ended
        val tails = nextBranches.flatMap(parseRecursive)
        
        // Check if we reached a dead end
        // If tails is empty, it means we couldn't parse further.
        // But if nextBranches lead to EOF, tails should contain EOF nodes.
        // If nextBranches was empty, it's also a dead end (premature end without EOF).
        
        if (tails.isEmpty && nextBranches.nonEmpty) {
           Vector.empty
        } else if (nextBranches.isEmpty) {
           Vector.empty
        } else {
           // Attach tails to the node
           node match {
             case Tree.V(v, b) => Vector(Tree.V(v, b ++ tails))
             case Tree.E() => tails
           }
        }
      }
    } else {
      Vector.empty
    }

    // 2. Shift / Skip (Standard traversal)
    // Non-deterministic: we also consider the path where we DON'T apply a rule
    // (or apply a rule at the next token).
    val shiftResults = cursor match {
      case Tree.V((_, _, _, "EOF"), _) =>
        // Found EOF, return it to signal success of this path
        Vector(Tree.V("EOF", Vector.empty))
        
      case Tree.V((_, _, _, "SOF"), branches) =>
        // SOF is skipped, just parse children
        branches.flatMap(parseRecursive)
        
      case Tree.V(token, branches) =>
        // Regular token. Consumed as a leaf (or node with children).
        val tails = branches.flatMap(parseRecursive)
        if (tails.isEmpty && branches.nonEmpty) {
          Vector.empty
        } else {
          // Use token content as value
          Vector(Tree.V(token._4, tails))
        }
        
      case Tree.E() => Vector.empty
    }
    
    // Combine both strategies
    ruleResults ++ shiftResults
  }
}
