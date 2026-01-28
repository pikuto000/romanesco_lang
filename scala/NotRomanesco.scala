/*package Notromanesco

final case class Token(kind:String,text:String)
type Tokens=Vector[Token]

enum AST{
  case Ident(name:String)
  case Add(left:AST,right:AST)
}

final class SimpleParser(tokens: Tokens) {
  import AST._
  
  private var pos: Int = 0
  
  def parseExpr(): AST = {
    var left = parseAtom()
    
    while (peekKind == "+") {
      consume("+")
      val right = parseAtom()
      left = Add(left, right)
    }
    
    left
  }
  
  private def parseAtom(): AST =
  peekKind match {
    case "ident" =>
    val t = consume("ident")
    Ident(t.text)
    case other =>
    error(s"unexpected token: $other")
  }
  
  private def peekKind: String =
  if (pos < tokens.length) tokens(pos).kind
  else "EOF"
  
  private def consume(expected: String): Token = {
    val t = tokens(pos)
    if (t.kind != expected)
    error(s"expected $expected, got ${t.kind}")
    pos += 1
    t
  }
  
  private def error(msg: String): Nothing =
  throw new RuntimeException(s"[pos=$pos] $msg")
}

@main def testSimpleParser(): Unit = 
  val tokens = Vector(
    Token("ident", "a"),
    Token("+", "+"),
    Token("ident", "b"),
    Token("+", "+"),
    Token("ident", "c")
  )

  val parser = new SimpleParser(tokens)
  val ast = parser.parseExpr()

  println(ast)
end testSimpleParser
*/