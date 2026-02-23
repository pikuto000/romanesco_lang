package romanesco

import scala.collection.mutable
import scala.collection.immutable
import scala.util.matching.Regex
import Utils.Debug.logger
import Parser._
import Types._
import romanesco.Utils.Debug
import romanesco.Utils.Macro

final class Registry {
  private type Token = Tuple4[UInt, UInt, Regex, String]
  private type TokenTree = Tree[Token]
  private var tokenizerHistory: Vector[Tokenizer] =
    Vector()

  private var parserHistory: Vector[rParser] =
    Vector()

  private var macroHistory: Vector[Macro[Any, Any]] =
    Vector()

  private var interpreterHistory: Vector[romanesco.Runtime.VM] =
    Vector()

  private var jitHistory: Vector[romanesco.Runtime.LLVMJIT] =
    Vector()

  private var speculativeHistory
      : Vector[romanesco.Runtime.SpeculativeExecutor] =
    Vector()

  /* ========= current ========= */

  def currentTokenizer: Tokenizer =
    tokenizerHistory.last

  def currentParser: rParser =
    parserHistory.last

  def currentMacro: Macro[Any, Any] =
    macroHistory.last

  def currentVM: romanesco.Runtime.VM =
    interpreterHistory.last

  def currentJIT: romanesco.Runtime.LLVMJIT =
    jitHistory.last

  def currentSpeculative: romanesco.Runtime.SpeculativeExecutor =
    speculativeHistory.last

  /* ========= random access ========= */

  def anyTokenizer(fromLast: Int = 0): Tokenizer = {
    val address = tokenizerHistory.size - 1 - fromLast
    if (address < 0 || address >= tokenizerHistory.size)
      throw new RuntimeException(
        s"Tokenizer history address $address is out of range"
      )
    tokenizerHistory(address)
  }

  def anyParser(fromLast: Int = 0): rParser = {
    val address = parserHistory.size - 1 - fromLast
    if (address < 0 || address >= parserHistory.size)
      throw new RuntimeException(
        s"Parser history address $address is out of range"
      )
    parserHistory(address)
  }

  def anyMacro(fromLast: Int = 0): Macro[Any, Any] = {
    val address = macroHistory.size - 1 - fromLast
    if (address < 0 || address >= macroHistory.size)
      throw new RuntimeException(
        s"Macro history address $address is out of range"
      )
    macroHistory(address)
  }

  def anyVM(fromLast: Int = 0): romanesco.Runtime.VM = {
    val address = interpreterHistory.size - 1 - fromLast
    if (address < 0 || address >= interpreterHistory.size)
      throw new RuntimeException(
        s"VM history address $address is out of range"
      )
    interpreterHistory(address)
  }

  def anyJIT(fromLast: Int = 0): romanesco.Runtime.LLVMJIT = {
    val address = jitHistory.size - 1 - fromLast
    if (address < 0 || address >= jitHistory.size)
      throw new RuntimeException(
        s"JIT history address $address is out of range"
      )
    jitHistory(address)
  }

  def anySpeculative(
      fromLast: Int = 0
  ): romanesco.Runtime.SpeculativeExecutor = {
    val address = speculativeHistory.size - 1 - fromLast
    if (address < 0 || address >= speculativeHistory.size)
      throw new RuntimeException(
        s"SpeculativeExecutor history address $address is out of range"
      )
    speculativeHistory(address)
  }

  /* ========= push ========= */

  def pushTokenizer(rules: immutable.Map[String, Regex]): Unit = {
    tokenizerHistory = tokenizerHistory :+ new Tokenizer(rules)
  }

  def pushParser(rules: immutable.Map[String, ParseRule]): Unit = {
    parserHistory = parserHistory :+ new rParser(rules)
  }

  def pushMacro(
      init: Any,
      expander: (Any, Any) => Any,
      expandrule: Any
  ): Unit = {
    macroHistory = macroHistory :+ new Macro(init, expander, expandrule)
  }

  def pushVM(vm: romanesco.Runtime.VM): Unit = {
    interpreterHistory = interpreterHistory :+ vm
  }

  def pushJIT(jit: romanesco.Runtime.LLVMJIT): Unit = {
    jitHistory = jitHistory :+ jit
  }

  def pushSpeculative(se: romanesco.Runtime.SpeculativeExecutor): Unit = {
    speculativeHistory = speculativeHistory :+ se
  }

  /* ========= dump ========= */

  def dumpTokenizer: Vector[Tokenizer] = tokenizerHistory
  def dumpParser: Vector[rParser] = parserHistory
  def dumpMacro: Vector[Macro[Any, Any]] = macroHistory

  /* ========= run ========= */

  def run(input: String): rParser#ParseTree = {
    val tokenTree = currentTokenizer.toknize(input)
    logger.log(tokenTree.prettyPrint())
    val ParseTree = currentParser.parse(tokenTree)
    logger.log(ParseTree.prettyPrint())
    // マクロはcurrentMacroかanyMacroを使って別途使用すること
    ParseTree
  }
}
/*
@main def testregistry: Unit = {
  import Predicates._
  Debug.logger.switch(false)
  
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
