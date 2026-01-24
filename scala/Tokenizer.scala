package romanesco
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.util.boundary
import scala.collection.mutable
import Debug.logger
final class Tokenizer(rules:Map[String,Regex])extends RegexParsers {
  override val skipWhitespace = false
  type TokenType = Regex
  type Content = String
  type Row = Int
  type Col = Int
  type Token = Tuple4[Row,Col,TokenType,Content]
  type TokenTree = Tree[Token]
  //ソースの内容を、解釈の可能性を全て残しながら再帰的にトークナイズする。
  //解釈の可能性を残すために、Vector[TokenTree]型を用いる。
  private val cache = mutable.Map[Int, Vector[TokenTree]]()
  def toknize(s:String)(row:Row=0,col:Col=0,offset:Int=0):Vector[TokenTree]={
    
    if (cache.contains(offset)) {
      logger.log(s"cache hit at offset $offset")
      return cache(offset)
    }

    val result = if (s.isEmpty) {
      Vector(Tree.V((row, col, "EOF".r, "EOF"), Vector.empty))
    } else {
      logger.log(s"tokenize start at line:${row+1} col:${col}, offset is $offset, string length:${s.length}")
      
      // 各ルールを適用して、マッチしたものをブランチとして追加する
      rules.values.flatMap { r =>
        parse(r, s) match {
          case Success(result, next) if result.length > 0 =>
            val new_row = row + result.count(_ == '\n')
            val new_col = if (result.contains("\n")) result.length - result.lastIndexOf("\n") - 1 else col + result.length
            val new_offset = offset + result.length
            val nextString = next.source.subSequence(next.offset, next.source.length).toString
            val subTrees = toknize(nextString)(new_row, new_col, new_offset)
            Some(Tree.V((row, col, r, result), subTrees))
          case _ => 
            None
        }
      }.toVector
    }

    cache(offset) = result
    result
  }
}
@main def testTokenize={
  //トークナイズのテスト
  Debug.logger.switch(true)
  val rules=Map(
  "comment" -> "//.*".r,
  "string" -> "\".*?\"".r,
  "excramation" -> "!".r,
  "semicolon" -> ";".r,
  "identifier" -> "[a-zA-Z_][a-zA-Z0-9_]*".r,
  "number" -> "[0-9]+".r,
  "float" -> "[0-9]+\\.[0-9]+".r,
  "whitespace" -> "\\s+".r,
  "word" -> "\\S+".r
  )
  val tokenizer = new Tokenizer(rules)
  val testString=
    """!test;"""
  val branches=tokenizer.toknize(testString)()
  val resultTree=Tree.V((0,0,"SOF".r,"SOF"), branches)

  println("\nTree structure:")
  println(resultTree.prettyPrint())
  
  println("\nFlattened paths (token contents):")
  resultTree.flatten.foreach { p =>
    println(p.map(t => s"'${t._4}'").mkString(" -> ").replace("\n","\\n").replace("\r","\\r"))
  }
}