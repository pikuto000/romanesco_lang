package romanesco
import scala.collection.immutable
import Predicates._
import Debug.logger

final class rParser(rules: immutable.Map[String, ParseRule]) {
  
  type Token = Tokenizer#Token
  type TokenTree = Tokenizer#TokenTree
  type Operator = String
  type Operand = Vector[String]
  type Parseresult = Tuple2[Operator, Operand]
  type ParseTree = Tree[Parseresult]

  def parse(tokenTree: TokenTree): ParseTree = {
    logger.log("Starting parsing...")
    // Start parsing. tokenTree root is SOF.
    val results:Vector[ParseTree] = parseRecursive(tokenTree)
    
    // Merge results into a single tree structure
    val merged:Vector[ParseTree] = Tree.merge(results)
    
    // Wrap in a Root node
    Tree.V(("ParseRoot", Vector.empty[String]), merged)
  }

  private def parseRecursive(cursor: Tree[Token]): Vector[ParseTree] = {
    // 1. Try to match rules at the current position (looking at children of cursor)
    val ruleMatches = rules.values.toVector.flatMap { rule =>
      rule.apply(cursor)
    }

    val ruleResults = ruleMatches.flatMap { case (node, nextBranches) =>
      // Continue parsing from the branches where the rule ended
      val tails = nextBranches.flatMap(parseRecursive)
      
      // Check if we reached a dead end
      // If nextBranches is not empty but tails is empty, it means we couldn't parse further.
      if (tails.isEmpty && nextBranches.nonEmpty) {
         Vector.empty
      } else {
         // Attach tails to the node. node is (Operator, Operand)
         // We also include operands as leaf nodes for visibility in the tree structure
         val operandNodes = node._2.map(s => Tree.V((s, Vector.empty[String]), Vector.empty))
         Vector(Tree.V(node, operandNodes ++ tails))
      }
    }

    // 2. Shift / Skip (Standard traversal)
    // Non-deterministic: we also consider the path where we DON'T apply a rule
    // (or apply a rule at the next token).
    val shiftResults = cursor match {
      case Tree.V((_, _, _, "EOF"), _) =>
        // Found EOF, return it as a result to signal success of this path
        Vector(Tree.V(("EOF", Vector.empty[String]), Vector.empty))
        
      case Tree.V((_, _, _, "SOF"), branches) =>
        // SOF is skipped, just parse children
        branches.flatMap(parseRecursive)
        
      case Tree.V(token, branches) =>
        // Regular token. Skip and recurse to find rules deeper in the tree.
        branches.flatMap(parseRecursive)
        
      case Tree.E() => Vector.empty
    }
    
    // Combine both strategies
    ruleResults ++ shiftResults
  }
}
