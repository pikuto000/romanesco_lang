// デバッグ用の簡易テストコード
package romanesco

object TestDebug {
  def main(args: Array[String]): Unit = {
    Hygenicmarker.reset()
    Macro.reset()
    logger.Switch(true)
    
    val source = """syntax test [X] = { X }
test 100"""
    
    println("=== SOURCE ===")
    println(source)
    println("==============\n")
    
    val lexer = init.lex.setup("testLexer")
    val parser = init.parse.setup("testParser", lexer)
    
    init.parse.literals(parser)
    init.parse.logic(parser)
    
    // レキシカル解析
    println("=== TOKENIZATION ===")
    val lattice = lexer.tokenize(source)
    lattice.toSeq.sortBy(_._1).foreach { case (offset, tokens) =>
      println(s"Offset $offset:")
      tokens.foreach(t => println(s"  - ${t.getClass.getSimpleName}: '${t.s}' (${t.tag.name})"))
    }
    println("====================\n")
    
    // パース
    println("=== PARSING ===")
    val parseResult = parser.apply(lattice.asInstanceOf[Map[Int, Array[parser.lexer.Token]]], source)
    
    parseResult match {
      case parser.Success(results, _) => {
        println("\n=== PARSE RESULTS ===")
        results.zipWithIndex.foreach { case (node, idx) =>
          println(s"[$idx] $node")
        }
        println("=====================\n")
        
        println("=== MACRO EXPANSION ===")
        val optimized = init.optimization.apply(results)
        optimized.zipWithIndex.foreach { case (node, idx) =>
          println(s"[$idx] $node")
        }
        println("=======================\n")
      }
        
      case _ => {
        println("PARSE FAILED")
      }
    }
  }
}
