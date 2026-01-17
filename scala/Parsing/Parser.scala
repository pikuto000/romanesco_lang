package Parsing

import Lexing.Token
import Undeterminable.tree
import scala.collection.mutable
import java.util.UUID

// ======================================
// Parsing Entry Point
// ======================================

object Parser:
  
  def parseAll(tokenTree: tree[Token], debug: Boolean = false): tree[List[Stmt]] =
    val paths = tokenTree.flattenPaths.map(_.toList)
    
    if paths.isEmpty then
      tree.DeadEnd
    else
      val results = paths.flatMap {
        case tokens =>
          val engine = new RewritingEngine(tokens, debug)
          val interpretations = engine.run()
          interpretations
      }
      
      if results.isEmpty then tree.DeadEnd
      else tree.fork(results.map(r => tree.leaf(r)))

// ======================================
// Rewriting Engine (Structural Mixfix)
// ======================================

case class Rule(id: String, pattern: List[PatternUnit], body: List[Token])

enum PatternUnit:
  case Param(name: String)
  case Word(value: String)

class RewritingEngine(initialTokens: List[Token], debug: Boolean = false):
  
  private val arities: Map[String, Int] = Map(
    "+" -> 2, "-" -> 2, "*" -> 2, "/" -> 2, "=" -> 2, "seq" -> 2, "lambda" -> 2,
    "true" -> 0, "false" -> 0, "and" -> 2, "or" -> 2,
    "==" -> 2, "!=" -> 2, ">" -> 2, "<" -> 2, ">=" -> 2, "<=" -> 2
  )
  
  private val reservedKeywords = mutable.Set("syntax", "->", "import") ++ arities.keySet

  def log(msg: String): Unit =
    if debug then println(s"[REWRITE] $msg")

  def run(): List[List[Stmt]] =
    val finalSequences = rewrite(initialTokens, Nil)
    
    val results = mutable.ListBuffer[List[Stmt]]()
    val seen = mutable.Set[List[String]]()
    
    for seq <- finalSequences do
      val filtered = seq.filterNot(_.isInstanceOf[Token.WS])
      val key = filtered.map(_.lexeme)
      if !seen.contains(key) then
        seen += key
        if debug then log(s"Stable state: ${key.mkString(" ")}")
        val ast = toAST(filtered)
        if ast.nonEmpty then results += ast
    
    results.toList

  private def rewrite(tokens: List[Token], rules: List[Rule]): List[List[Token]] =
    if tokens.isEmpty then return List(Nil)
    
    val firstNonWSIdx = tokens.indexWhere(!_.isInstanceOf[Token.WS])
    if (firstNonWSIdx != -1 && tokens(firstNonWSIdx).lexeme == "syntax") then
      val (rule, rest) = extractSyntaxRule(tokens.drop(firstNonWSIdx)) match
        case Some(res) => res
        case None => return List(tokens) // Fail to parse syntax, keep as is
      
      // Update reserved keywords with new macro trigger words
      rule.pattern.foreach {
        case PatternUnit.Word(w) => reservedKeywords += w
        case _ => 
      }
      
      log(s"Registered macro: ${rule.pattern.collect{case PatternUnit.Word(w) => w}.mkString(" ")}")
      return rewrite(tokens.take(firstNonWSIdx) ++ rest, rules :+ rule)

    // 2. Structural Rewriting
    val statements = splitStatements(tokens)
    val rewrittenStatements = statements.map(s => rewriteStatement(s, rules))
    
    List(rewrittenStatements.flatten)

  private def splitStatements(tokens: List[Token]): List[List[Token]] =
    val stmts = mutable.ListBuffer[List[Token]]()
    var curr = mutable.ListBuffer[Token]()
    var depth = 0
    
    for t <- tokens do
      t match
        case Token.Delim("(") | Token.Delim("{") | Token.Delim("[") => depth += 1
        case Token.Delim(")") | Token.Delim("}") | Token.Delim("]") => depth -= 1
        case _ => 
      
      curr += t
      if depth == 0 then
        t match
          case Token.WS(s) if s.contains("\n") =>
            stmts += curr.toList
            curr = mutable.ListBuffer()
          case Token.Delim(";") =>
            stmts += curr.toList
            curr = mutable.ListBuffer()
          case _ =>
    
    if curr.nonEmpty then stmts += curr.toList
    stmts.toList

  private def rewriteStatement(tokens: List[Token], rules: List[Rule]): List[Token] =
    var curr = tokens
    var changed = true
    var iterations = 0
    
    while changed && iterations < 50 do
      changed = false
      iterations += 1
      val units = getUnits(curr)
      
      var i = 0
      var applied = false
      while i < units.length && !applied do
        for rule <- rules if !applied do
          matchRule(rule, units.drop(i), i) match
            case Some((consumedUnits, binds)) =>
              val before = units.take(i).flatten
              val after = units.drop(i + consumedUnits).flatten
              val expanded = applyRule(rule, binds)
              
              log(s"Applied macro: ${curr.filterNot(_.isInstanceOf[Token.WS]).map(_.lexeme).take(10).mkString(" ")}")
              
              curr = before ++ expanded ++ after
              changed = true
              applied = true
            case None =>
        i += 1
    curr

  private def getUnits(tokens: List[Token]): List[List[Token]] =
    val units = mutable.ListBuffer[List[Token]]()
    var i = 0
    while i < tokens.length do
      if tokens(i).isInstanceOf[Token.WS] then
        units += List(tokens(i))
        i += 1
      else
        val len = matchOneExpression(tokens.drop(i))
        if len > 0 then
          units += tokens.slice(i, i + len)
          i += len
        else
          units += List(tokens(i))
          i += 1
    units.toList

  private def matchRule(rule: Rule, units: List[List[Token]], unitIndex: Int): Option[(Int, Map[String, List[Token]])] =
    // Infix-like check: trigger word not at start of statement
    if rule.pattern.head.isInstanceOf[PatternUnit.Param] then
      if unitIndex == 0 then return None
      // Also check if preceded by an operator
      // (Simplified: in getUnits, we already have logical units)
    
    var uPtr = 0
    var pPtr = 0
    val binds = mutable.Map[String, List[Token]]()
    
    while pPtr < rule.pattern.length do
      // Skip WS units
      while uPtr < units.length && units(uPtr).forall(_.isInstanceOf[Token.WS]) do
        uPtr += 1
      
      if uPtr >= units.length then return None
      
      rule.pattern(pPtr) match
        case PatternUnit.Param(name) =>
          val firstTok = units(uPtr).head
          // A parameter unit must be a full expression, not a structural keyword
          if firstTok.lexeme != "(" && firstTok.lexeme != "{" && reservedKeywords.contains(firstTok.lexeme) then
            return None
          
          binds(name) = units(uPtr)
          uPtr += 1
        case PatternUnit.Word(value) =>
          if units(uPtr).head.lexeme != value then return None
          uPtr += 1
      pPtr += 1
    
    Some((uPtr, binds.toMap))

  private def matchOneExpression(tokens: List[Token]): Int =
    if tokens.isEmpty then return 0
    
    var start = 0
    while start < tokens.length && tokens(start).isInstanceOf[Token.WS] do
      if tokens(start).lexeme.contains("\n") then return 0
      start += 1
    if start >= tokens.length then return 0
    
    val first = tokens(start)
    first match
      case Token.Delim("(") | Token.Delim("{") =>
        var depth = 0
        var idx = start
        val open = first.lexeme
        val close = if open == "(" then ")" else "}"
        var balanced = false
        while idx < tokens.length && !balanced do
          val t = tokens(idx)
          if t.lexeme == open then depth += 1
          else if t.lexeme == close then depth -= 1
          idx += 1
          if depth == 0 then balanced = true
        if balanced then idx else 0
      
      case _ if arities.contains(first.lexeme) =>
        val arity = arities(first.lexeme)
        var consumed = start + 1
        var possible = true
        for _ <- 0 until arity if possible do
          val argLen = matchOneExpression(tokens.drop(consumed))
          if argLen == 0 then possible = false
          else consumed += argLen
        if possible then consumed else 0
      
      case _ if reservedKeywords.contains(first.lexeme) => 0
      
      case Token.Delim(_) => 0 // Other delims are not start of expr
      
      case _ => start + 1 // Atomic

  private def extractSyntaxRule(tokens: List[Token]): Option[(Rule, List[Token])] =
    val arrowIdx = tokens.indexWhere(_.lexeme == "->")
    if arrowIdx == -1 then return None
    
    val pattern = parsePattern(tokens.slice(1, arrowIdx).filterNot(_.isInstanceOf[Token.WS]))
    if pattern.isEmpty then return None
    
    val bodyStart = arrowIdx + 1
    var bodyEnd = bodyStart
    var hasContent = false
    var foundEnd = false
    while bodyEnd < tokens.length && !foundEnd do
      val t = tokens(bodyEnd)
      if t.lexeme == "syntax" then foundEnd = true
      else
        t match
          case Token.WS(s) if s.contains("\n") && hasContent => foundEnd = true
          case Token.WS(_) => // ignore
          case _ => hasContent = true
        if !foundEnd then bodyEnd += 1
    
    Some((Rule(UUID.randomUUID().toString, pattern, tokens.slice(bodyStart, bodyEnd)), tokens.drop(bodyEnd)))

  private def parsePattern(toks: List[Token]): List[PatternUnit] =
    val res = mutable.ListBuffer[PatternUnit]()
    var i = 0
    while i < toks.length do
      toks(i).lexeme match
        case "[" =>
          if i + 2 < toks.length && toks(i + 2).lexeme == "]" then
            res += PatternUnit.Param(toks(i + 1).lexeme)
            i += 3
          else return Nil // invalid
        case word =>
          res += PatternUnit.Word(word)
          i += 1
    res.toList

  private def applyRule(rule: Rule, binds: Map[String, List[Token]]): List[Token] =
    rule.body.flatMap {
      case t => binds.getOrElse(t.lexeme, List(t))
    }

  private def toAST(tokens: List[Token]): List[Stmt] =
    var ptr = 0
    
    def parseOne(): Expr =
      if ptr >= tokens.length then return Expr.Var("null")
      val t = tokens(ptr)
      ptr += 1
      
      t.lexeme match
        case "(" =>
          val inner = mutable.ListBuffer[Token]()
          var depth = 1
          while ptr < tokens.length && depth > 0 do
            if tokens(ptr).lexeme == "(" then depth += 1
            else if tokens(ptr).lexeme == ")" then depth -= 1
            if depth > 0 then inner += tokens(ptr)
            ptr += 1
          
          val interpretations = new RewritingEngine(inner.toList, false).run()
          if interpretations.nonEmpty && interpretations.head.nonEmpty then
            interpretations.head.head match
              case Stmt.ExprStmt(e) => 
                if interpretations.head.length > 1 then
                  val args = interpretations.head.drop(1).collect { case Stmt.ExprStmt(arg) => arg }
                  Expr.Call(e, args)
                else e
          else Expr.Var("null")

        case "{" =>
          val inner = mutable.ListBuffer[Token]()
          var depth = 1
          while ptr < tokens.length && depth > 0 do
            if tokens(ptr).lexeme == "{" then depth += 1
            else if tokens(ptr).lexeme == "}" then depth -= 1
            if depth > 0 then inner += tokens(ptr)
            ptr += 1
          
          val interpretations = new RewritingEngine(inner.toList, false).run()
          if interpretations.nonEmpty then
            Expr.Block(interpretations.head.map { case Stmt.ExprStmt(e) => e })
          else Expr.Block(Nil)

        case "lambda" =>
          if ptr + 1 < tokens.length then
            val param = tokens(ptr).lexeme
            ptr += 1
            Expr.Lambda(param, parseOne())
          else Expr.Var("null")

        case lexeme if arities.getOrElse(lexeme, 0) > 0 =>
          val arity = arities(lexeme)
          val args = (0 until arity).map(_ => parseOne()).toList
          Expr.Call(Expr.Var(lexeme), args)
        
        case lexeme =>
          t match
            case Token.Number(v) => Expr.Num(v)
            case _ => Expr.Var(lexeme)

    val stmts = mutable.ListBuffer[Stmt]()
    while ptr < tokens.length do
      val expr = parseOne()
      stmts += Stmt.ExprStmt(expr)
    stmts.toList