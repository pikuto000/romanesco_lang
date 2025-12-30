package parser
import parser.Node
import parser.Apply
import parser.Atom
import parser.SymbolTable
import parser.interpreter

object init {
  def strict(f: Array[Any] => Any): (Array[Node], SymbolTable) => Any = {
    (nodes, s) => f(nodes.map(interpreter.eval(_, s)))
  }

  def injectMacroTools(sym: SymbolTable): Unit = {
    sym.set("get-arg", Apply("get-arg", Array(Atom("I")), (args, s) => {
      s.get("__macro_args__") match {
        case Some(Apply(_, nodes, _, _)) => nodes(interpreter.eval(args(0), s).asInstanceOf[BigDecimal].toInt)
        case _ => throw new RuntimeException("get-arg error")
      }
    }))

    sym.set("make-apply", Apply("make-apply", Array(Atom("F"), Atom("A")), (args, s) => {
      val funVal = interpreter.eval(args(0), s)
      val argNodesVal = interpreter.eval(args(1), s)
      
      def toNode(v: Any): Node = v match {
        case n: Node => n
        case other => Atom(other.toString)
      }

      val nodes: Array[Node] = argNodesVal match {
        case a: Array[Node] => a
        case app: Apply if app.fun == "cons" => 
          def collect(n: Any): List[Node] = n match {
            case a: Apply if a.fun == "cons" => toNode(interpreter.eval(a.args(0), s)) :: collect(interpreter.eval(a.args(1), s))
            case _ => Nil
          }
          collect(app).toArray
        case other => Array(toNode(other))
      }

      val funName = funVal match { case n: Node => n.rawName case other => other.toString }
      s.get(funName) match {
        case Some(app: Apply) => Apply(funName, nodes, app.func, app.customParser)
        case _ => throw new RuntimeException(s"make-apply: '$funName' not found")
      }
    }))

    sym.set("make-atom", Apply("make-atom", Array(Atom("V")), (args, s) => Atom(interpreter.eval(args(0), s).toString)))
    sym.set("quote", Apply("quote", Array(Atom("N")), (args, s) => args(0), None), Map("phase" -> "ast"))
  }

  def core(): SymbolTable = {
    val sym = new SymbolTable
    val bs = 92.toChar.toString
    sym.lex.addDelimiter('('); sym.lex.addDelimiter(')'); sym.lex.addDelimiter(bs(0))
    injectMacroTools(sym)

    sym.set("nil", Apply("nil", Array(), (a, s) => Atom("nil")))
    
    // cons: (H T) -> returns a data node representing the pair
    sym.set("cons", Apply("cons", Array(Atom("H"), Atom("T")), (args, s) => {
      Apply("cons", args, (a, sy) => ())
    }))

    sym.set("head", Apply("head", Array(Atom("L")), (args, s) => {
      interpreter.eval(args(0), s) match { case a: Apply if a.fun == "cons" => a.args(0) case _ => throw new RuntimeException(s"head error") }
    }))
    sym.set("tail", Apply("tail", Array(Atom("L")), (args, s) => {
      interpreter.eval(args(0), s) match { case a: Apply if a.fun == "cons" => a.args(1) case _ => throw new RuntimeException(s"tail error") }
    }))

    sym.set("+", Apply("+", Array(Atom("L"), Atom("R")), strict(args => args(0).asInstanceOf[BigDecimal] + args(1).asInstanceOf[BigDecimal])))
    
    sym.set(":-", Apply(":-", Array(Atom("N"), Atom("C"), Atom("E")), (args, s) => {
      val name = interpreter.eval(args(0), s).toString
      val value = interpreter.eval(args(2), s)
      s.set(name, value); value
    }))

    sym.set(bs, Apply(bs, Array(Atom("A"), Atom("E"), Atom("X")), (args, s) => {
      val argName = args(0) match { case n: Node => n.rawName case _ => args(0).toString }
      val body = args(1); val applyArg = args(2); val capturedSym = s
      (nodes: Array[Node], callerSym: SymbolTable) => {
        val argVal = interpreter.eval(nodes(0), callerSym)
        val lSym = capturedSym.extend()
        lSym.set(argName, argVal)
        interpreter.eval(body, lSym)
      }
    }))

    sym.set("macro-ast", Apply("macro-ast", Array(Atom("N"), Atom("B")), (args, s) => {
      val name = interpreter.eval(args(0), s).toString; val bodyNode = args(1)
      s.set(name, Apply(name, Array(Atom("Args")), (a, callerSym) => {
        val mSym = callerSym.extend(); injectMacroTools(mSym)
        mSym.set("__macro_args__", Apply("__macro_args__", a, (x, y) => ()))
        val f = interpreter.eval(bodyNode, mSym).asInstanceOf[(Array[Node], SymbolTable) => Any]
        f(a, mSym)
      }), Map("phase" -> "ast"))
      name
    }))

    sym.set("write", Apply("write", Array(Atom("E")), (args, s) => { val v = interpreter.eval(args(0), s); println(v); v }))
    sym
  }

  def setup: SymbolTable = core()
}