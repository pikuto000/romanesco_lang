package romanesco

import scala.collection.mutable
import scala.util.matching.Regex
type Token = Tokenizer#Token

final class Registory {
  private var tokenizerHistory: Vector[Tokenizer] =
  Vector.empty
  
  private var parserHistory: Vector[rParser[Any, Any]] =
  Vector.empty
  
  /* ========= current ========= */
  
  def currentTokenizer: Tokenizer =
  tokenizerHistory.last
  
  def currentParser: rParser[Any, Any] =
  parserHistory.last
  
  /* ========= random access ========= */
  
  def anyTokenizer(fromLast: Int = 0): Tokenizer = {
    val address = tokenizerHistory.size - 1 - fromLast
    if (address < 0 || address >= tokenizerHistory.size)
    throw new RuntimeException(
    s"Tokenizer history address $address is out of range"
    )
    tokenizerHistory(address)
  }
  
  def anyParser(fromLast: Int = 0): rParser[Any, Any] = {
    val address = parserHistory.size - 1 - fromLast
    if (address < 0 || address >= parserHistory.size)
    throw new RuntimeException(
    s"Parser history address $address is out of range"
    )
    parserHistory(address)
  }
  
  /* ========= push ========= */
  
  def pushTokenizer(rules: Map[String, Regex]): Unit = {
    tokenizerHistory =
    tokenizerHistory :+ new Tokenizer(rules)
  }
  
  def pushParser(
  rules: mutable.Map[String, ParseRule[Any, Any]]
  ): Unit = {
    parserHistory =
    parserHistory :+ new rParser(rules)
  }
  
  /* ========= dump ========= */
  
  def dumpTokenizer: Vector[Tokenizer] =
  tokenizerHistory
  
  def dumpParser: Vector[rParser[Any, Any]] =
  parserHistory
  
  def run(input: String): Tree[Any] = {
    val tokenTree = currentTokenizer.toknize(input)
    currentParser.parse(tokenTree.asInstanceOf[Tree[Any]])
  }
}
/*
@main def testregistry: Unit = {
  import Predicates._
  Debug.logger.switch(true)
  
  val reg = new Registory()
  
  // 1. Setup Tokenizer
  val tokenRules = Map(
  "hello" -> "hello".r,
  "world" -> "world".r,
  "space" -> "\\s+".r
  )
  reg.pushTokenizer(tokenRules)
  
  // 2. Setup Parser
  // Define a rule: Greeting -> hello world
  val ruleHello = new StandardRule[Any, Any](
  name = "Greeting",
  pattern = Vector(
  matches { case (_, _, _, content: String) => content == "hello" case _ => false },
  matches { case (_, _, _, content: String) => content == "world" case _ => false }
  ),
  build = { children =>
    // children(0) is hello, children(1) is world
    Tree.V("GreetingMatched", Vector.empty) // New node
  }
  )
  
  // Ignore spaces rule (optional, or just handle in pattern if tokens are adjacent)
  // Since Tokenizer produces a graph of ALL tokens including spaces, 
  // if we want to skip spaces, we need rules that consume spaces or a pre-filter.
  // For this simple test, I'll just use "helloworld" or input "hello world" but match space explicitly if needed.
  // Let's try input "helloworld" to avoid space handling complexity for now, 
  // or add a space predicate.
  
  // Let's just test "helloworld" (two tokens adjacent)
  
  val rules = mutable.Map[String, ParseRule[Any, Any]]()
  rules("Greeting") = ruleHello
  
  reg.pushParser(rules)
  
  // 3. Run
  println("--- Running Parser Test ---")
  val input = "helloworld"
  val result = reg.run(input)
  
  println("--- Result ---")
  println(result.prettyPrint())
}
*/