package parser
import parser.Node
import parser.Apply
import parser.SymbolTable
import parser.interpreter

object init {
  // --- Helper for strict evaluation with Auto-Currying logic ---
  def strict(f: Array[Any] => Any): (Array[Node], SymbolTable) => Any = {
    def wrapper(nodes: Array[Node], s: SymbolTable): Any = {
      val hasPlaceholder = nodes.exists {
        case Atom("_") => true
        case _ => false
      }

      if (hasPlaceholder) {
        // Return a partially applied function (Closure)
        (extraNodes: Array[Node], callerSym: SymbolTable) => {
          var extraIdx = 0
          val mergedNodes = nodes.map {
            case Atom("_") if extraIdx < extraNodes.length =>
              val n = extraNodes(extraIdx)
              extraIdx += 1
              n
            case other => other
          }
          wrapper(mergedNodes, callerSym)
        }
      } else {
        val args = nodes.map(interpreter.eval(_, s))
        f(args)
      }
    }
    wrapper
  }

  // --- Macro-only Utilities ---
  // Registered in core() so parser knows arity, but execution is restricted.
  private def ensureMacroContext(s: SymbolTable, name: String): Unit = {
    if (s.get("__macro_args__").isEmpty) {
      throw new RuntimeException(s"'$name' can only be used inside a macro")
    }
  }

  def injectMacroTools(sym: SymbolTable): Unit = {
    sym.set("get-arg", Apply("get-arg", Array(Atom("Index")), (args, s) => {
      ensureMacroContext(s, "get-arg")
      val idx = interpreter.eval(args(0), s).asInstanceOf[BigDecimal].toInt
      s.get("__macro_args__") match {
        case Some(Apply(_, nodes, _)) => nodes(idx)
        case _ => throw new RuntimeException("Macro arguments not found")
      }
    }))

    sym.set("eval", Apply("eval", Array(Atom("Node")), (args, s) => {
      ensureMacroContext(s, "eval")
      val node = interpreter.eval(args(0), s).asInstanceOf[Node]
      interpreter.eval(node, s)
    }))

    sym.set("make-apply", Apply("make-apply", Array(Atom("Fun"), Atom("Args")), (args, s) => {
      ensureMacroContext(s, "make-apply")
      val fun = interpreter.eval(args(0), s).toString
      val nodes = interpreter.eval(args(1), s).asInstanceOf[Array[Node]]
      Apply(fun, nodes, s.getFunc(fun))
    }))

    sym.set("make-atom", Apply("make-atom", Array(Atom("Value")), (a, s) => {
      ensureMacroContext(s, "make-atom")
      Atom(interpreter.eval(a(0), s).toString)
    }))

    sym.set("gensym", Apply("gensym", Array(), (a, s) => {
      ensureMacroContext(s, "gensym")
      s.gensym()
    }))
  }

  // Runtime core functions
  def core(): SymbolTable = {
    val sym = new SymbolTable
    
    // Register tools in core so parser knows their arity
    injectMacroTools(sym)

    sym.set("+", Apply("+", Array(Atom("Left"), Atom("Right")), strict(args => {
      args(0).asInstanceOf[BigDecimal] + args(1).asInstanceOf[BigDecimal]
    })))

    sym.set("-", Apply("-", Array(Atom("Left"), Atom("Right")), strict(args => {
      args(0).asInstanceOf[BigDecimal] - args(1).asInstanceOf[BigDecimal]
    })))

    sym.set("*", Apply("*", Array(Atom("Left"), Atom("Right")), strict(args => {
      args(0).asInstanceOf[BigDecimal] * args(1).asInstanceOf[BigDecimal]
    })))

    sym.set("/", Apply("/", Array(Atom("Left"), Atom("Right")), strict(args => {
      args(0).asInstanceOf[BigDecimal] / args(1).asInstanceOf[BigDecimal]
    })))

    sym.set("%", Apply("%", Array(Atom("Left"), Atom("Right")), strict(args => {
      args(0).asInstanceOf[BigDecimal] % args(1).asInstanceOf[BigDecimal]
    })))

    sym.set("^", Apply("^", Array(Atom("Left"), Atom("Right")), strict(args => {
      args(0).asInstanceOf[BigDecimal].pow(args(1).asInstanceOf[BigDecimal].toInt)
    })))

    sym.set("case", Apply("case", Array(Atom("Cond"), Atom("Then"), Atom("Else")), (args, s) => {
      val cond = interpreter.eval(args(0), s).asInstanceOf[Boolean]
      if (cond) interpreter.eval(args(1), s) else interpreter.eval(args(2), s)
    }))

    sym.set("==", Apply("==", Array(Atom("Left"), Atom("Right")), strict(args => args(0) == args(1))))
    sym.set("!=", Apply("!=", Array(Atom("Left"), Atom("Right")), strict(args => args(0) != args(1))))

    sym.set("<", Apply("<", Array(Atom("Left"), Atom("Right")), strict(args => {
      args(0).asInstanceOf[BigDecimal] < args(1).asInstanceOf[BigDecimal]
    })))

    sym.set(">", Apply(">", Array(Atom("Left"), Atom("Right")), strict(args => {
      args(0).asInstanceOf[BigDecimal] > args(1).asInstanceOf[BigDecimal]
    })))

    sym.set("=<", Apply("<=", Array(Atom("Left"), Atom("Right")), strict(args => {
      args(0).asInstanceOf[BigDecimal] <= args(1).asInstanceOf[BigDecimal]
    })))

    sym.set(">=", Apply(">=", Array(Atom("Left"), Atom("Right")), strict(args => {
      args(0).asInstanceOf[BigDecimal] >= args(1).asInstanceOf[BigDecimal]
    })))

    sym.set(":-", Apply(":-", Array(Atom("Name"), Atom("Constraint/Type"), Atom("Expr")), (args, s) => {
      val name = args(0).asInstanceOf[Atom].s
      val expr = interpreter.eval(args(2), s)
      val funcOpt = expr match {
        case f: Function2[?, ?, ?] => Some(f.asInstanceOf[(Array[Node], SymbolTable) => Any])
        case _ => None
      }
      if (funcOpt.isDefined) {
        s.set(name, Apply(name, Array(Atom("Arg")), funcOpt.get))
      } else {
        s.set(name, Apply(name, Array(), (nodes, s2) => expr))
      }
      expr
    }))

    sym.set("\\", Apply("\\", Array(Atom("Arg"), Atom("Expr"), Atom("ApplyArg")), (args, s) => {
      val argName = args(0) match {
        case Atom(name) => name
        case _ => throw new IllegalArgumentException(s"Lambda argument must be an Atom")
      }
      val body = args(1)
      val applyArg = args(2)
      val capturedSym = s
      val lambdaFunc: (Array[Node], SymbolTable) => Any = (lambdaNodes, callerSym) => {
        if (lambdaNodes.length != 1) throw new IllegalArgumentException("Lambda expects 1 argument")
        val argVal = interpreter.eval(lambdaNodes(0), callerSym)
        val lambdaSym = capturedSym.cloneTable()
        callerSym.get("__macro_args__").foreach(v => lambdaSym.set("__macro_args__", v))
        lambdaSym.set(argName, Apply("val", Array(), (nodes, s) => argVal))
        interpreter.eval(body, lambdaSym)
      }
      applyArg match {
        case Atom("_") => lambdaFunc
        case _ => lambdaFunc(Array(applyArg), s)
      }
    }))

    // --- Macro Registration Functions ---
    // These belong to the runtime core, but they enable macro expansion

    sym.set("macro-ast", Apply("macro-ast", Array(Atom("Name"), Atom("Body")), (args, s) => {
      val name = args(0).asInstanceOf[Atom].s
      
      // Create a context for evaluating the macro body during registration
      val registrationSym = s.extend()
      injectMacroTools(registrationSym)
      registrationSym.set("__macro_args__", Apply("__macro_args__", Array(), (x, y) => ()))
      
      val bodyResult = interpreter.eval(args(1), registrationSym)
      
      bodyResult match {
        case func: Function2[Array[Node], SymbolTable, Any] @unchecked =>
          s.set(name, Apply(name, Array(Atom("Args")), (a, sym) => {
            val macroSym = sym.extend()
            injectMacroTools(macroSym)
            macroSym.set("__macro_args__", Apply("__macro_args__", a, (x, y) => ()))
            func(a, macroSym)
          }))
        case node: Node =>
          s.set(name, Apply(name, Array(), (a, sym) => node))
        case value =>
          s.set(name, Apply(name, Array(), (a, sym) => Atom(value.toString)))
      }
      Expander.registerAstMacro(name)
      name
    }))

    sym.set("comptime", Apply("comptime", Array(Atom("Name"), Atom("Body")), (args, s) => {
      val name = args(0).asInstanceOf[Atom].s
      
      val registrationSym = s.extend()
      injectMacroTools(registrationSym)
      registrationSym.set("__macro_args__", Apply("__macro_args__", Array(), (x, y) => ()))
      
      val bodyResult = interpreter.eval(args(1), registrationSym)
      
      bodyResult match {
        case func: Function2[Array[Node], SymbolTable, Any] @unchecked =>
          s.set(name, Apply(name, Array(Atom("Args")), (a, sym) => {
            val macroSym = sym.extend()
            injectMacroTools(macroSym)
            macroSym.set("__macro_args__", Apply("__macro_args__", a, (x, y) => ()))
            func(a, macroSym)
          }))
        case value =>
          s.set(name, Apply(name, Array(), (a, sym) => value))
      }
      Expander.registerComptimeMacro(name)
      name
    }))

    sym.set("macro-char", Apply("macro-char", Array(Atom("Char"), Atom("Func")), (args, s) => {
      val char = args(0).asInstanceOf[Atom].s
      val funcVal = interpreter.eval(args(1), s)
      Expander.registerCharMacro(char)
      s.set(char, Apply(char, Array(), (a, sym) => funcVal))
      char
    }))

    sym
  }

  // IO functions (Side effects)
  def registerIO(sym: SymbolTable): Unit = {
    def strict(f: Array[Any] => Any): (Array[Node], SymbolTable) => Any = {
      (nodes, s) => f(nodes.map(interpreter.eval(_, s)))
    }
    sym.set("write", Apply("write", Array(Atom("Expr")), strict(args => {
      println(args(0))
      args(0)
    })))
    sym.set("abort", Apply("abort", Array(), strict(args => {
      println("Aborted.")
      sys.exit(1)
    })))
  }

  def setup: SymbolTable = {
    val sym = core()
    registerIO(sym)
    sym
  }
}