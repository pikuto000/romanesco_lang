package parser
import parser.Node
import parser.Apply
import parser.SymbolTable
import parser.interpreter

object init {
  private def nameOf(v: Any): String = v match { case Atom(s) => s case s: String => s case other => other.toString }

  def injectMacroTools(sym: SymbolTable): Unit = {
    sym.set("get-env", Apply("get-env", Array(), (a, s) => s))
    sym.set("get-macro-env", Apply("get-macro-env", Array(), (a, s) => {
      s.get("__macro_env__").getOrElse(throw new RuntimeException("no macro env"))
    }))
    sym.set("get-arg", Apply("get-arg", Array(Atom("I")), (args, s) => {
      val idx = interpreter.eval(args(0), s).asInstanceOf[BigDecimal].toInt
      s.get("__macro_args__").collect { case a: Array[Node] => a(idx) }.getOrElse(throw new RuntimeException("no macro args"))
    }))
    sym.set("set-in", Apply("set-in", Array(Atom("E"), Atom("N"), Atom("V")), (args, s) => {
      val env = interpreter.eval(args(0), s).asInstanceOf[SymbolTable]
      val name = nameOf(interpreter.eval(args(1), s))
      val value = interpreter.eval(args(2), s) match { case n: Node => n case v => v }
      env.set(name, value); value
    }))
  }

  def core(): SymbolTable = {
    val sym = new SymbolTable
    val bs = 92.toChar.toString
    sym.tokens.addSpecial("("); sym.tokens.addSpecial(")"); sym.tokens.addSpecial("?")
    injectMacroTools(sym)

    sym.set("nil", Apply("nil", Array(), (a, s) => Atom("nil")))
    sym.set("cons", Apply("cons", Array(Atom("H"), Atom("T")), (a, s) => Apply("cons", a, sym.getFunc("cons"))))
    
    sym.set("head", Apply("head", Array(Atom("L")), (a, s) => {
      interpreter.eval(a(0), s) match {
        case Apply("cons", inner, _, _) => inner(0)
        case other => throw new RuntimeException(s"head needs cons, but got $other")
      }
    }))
    sym.set("tail", Apply("tail", Array(Atom("L")), (a, s) => {
      interpreter.eval(a(0), s) match {
        case Apply("cons", inner, _, _) => inner(1)
        case other => throw new RuntimeException(s"tail needs cons, but got $other")
      }
    }))

    sym.set("write", Apply("write", Array(Atom("V")), (a, s) => { val v = interpreter.eval(a(0), s); println(v); v }))

    // --- macro-reader: [startSymbol] [endSymbol] [transformerLambda] ---
    sym.set("macro-reader", Apply("macro-reader", Array(Atom("S"), Atom("E"), Atom("L")), (args, s) => {
      val start = nameOf(interpreter.eval(args(0), s))
      val end = nameOf(interpreter.eval(args(1), s))
      val transformer = interpreter.eval(args(2), s).asInstanceOf[(Array[Node], SymbolTable) => Any]

      s.tokens.addSpecial(start)
      s.tokens.addSpecial(end)

      val custom: CustomParser = (p, in) => {
        val result = p.rep(p.expr)(in)
        result match {
          case p.Success(nodes, next) =>
            p.lit(end)(next) match {
              case p.Success(_, after) =>
                def toCons(list: List[Node]): Node = list match {
                  case h :: t => Apply("cons", Array(h, toCons(t)), sym.getFunc("cons"))
                  case Nil => sym.get("nil").get.asInstanceOf[Node]
                }
                val res = transformer(Array(toCons(nodes)), s)
                p.Success(res.asInstanceOf[Node], after)
              case _ => p.Failure(s"Expected '$end'", next)
            }
          case ns: p.NoSuccess => ns.asInstanceOf[p.ParseResult[Node]]
        }
      }

      s.set(start, Apply(start, Array(), (a, s) => (), Some(custom)))
      start
    }))

    // --- Macros ---
    sym.set(":-", Apply(":-", Array(Atom("N"), Atom("C"), Atom("E")), (args, s) => {
      val name = nameOf(args(0))
      val value = interpreter.eval(args(2), s) match { case n: Node => n case v => v }
      s.set(name, value); value
    }), Map("phase" -> "ast"))

    sym.set(bs, Apply(bs, Array(Atom("A"), Atom("B"), Atom("X")), (args, s) => {
      val argName = nameOf(args(0)); val body = args(1); val applyArg = args(2); val captured = s
      val lambdaFunc = (nodes: Array[Node], callerSym: SymbolTable) => {
        val argVal = interpreter.eval(nodes(0), callerSym)
        val lSym = captured.extend()
        callerSym.get("__macro_env__").foreach(v => lSym.set("__macro_env__", v))
        callerSym.get("__macro_args__").foreach(v => lSym.set("__macro_args__", v))
        lSym.set(argName, argVal)
        interpreter.eval(body, lSym)
      }
      if (applyArg == Atom("_")) lambdaFunc
      else lambdaFunc(Array(applyArg), s)
    }), Map("phase" -> "ast"))

    sym.set("macro-ast", Apply("macro-ast", Array(Atom("Name"), Atom("Impl")), (args, s) => {
      val name = nameOf(interpreter.eval(args(0), s))
      val impl = interpreter.eval(args(1), s).asInstanceOf[(Array[Node], SymbolTable) => Any]
      
      s.set(name, Apply(name, Array(Atom("Args")), (a, callerSym) => {
        val macroSym = callerSym.extend(); injectMacroTools(macroSym)
        macroSym.set("__macro_env__", callerSym)
        macroSym.set("__macro_args__", a)
        impl(a, macroSym)
      }), Map("phase" -> "ast"))
      name
    }), Map("phase" -> "ast"))

    sym
  }

  def setup: SymbolTable = core()
}