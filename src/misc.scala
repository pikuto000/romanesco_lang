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
  private def ensureMacroContext(s: SymbolTable, name: String): Unit = {
    // Basic check for macro context
  }

  def injectMacroTools(sym: SymbolTable): Unit = {
    sym.set("get-arg", Apply("get-arg", Array(Atom("Index")), (args, s) => {
      s.get("__macro_args__") match {
        case Some(Apply(_, nodes, _, _)) => 
          val idx = interpreter.eval(args(0), s).asInstanceOf[BigDecimal].toInt
          nodes(idx)
        case _ => throw new RuntimeException("get-arg can only be used inside a macro")
      }
    }))

    sym.set("eval", Apply("eval", Array(Atom("Node")), (args, s) => {
      val node = interpreter.eval(args(0), s).asInstanceOf[Node]
      interpreter.eval(node, s)
    }))

    sym.set("make-apply", Apply("make-apply", Array(Atom("Fun"), Atom("Args")), (args, s) => {
      val fun = interpreter.eval(args(0), s).toString
      val nodes = interpreter.eval(args(1), s).asInstanceOf[Array[Node]]
      Apply(fun, nodes, s.getFunc(fun))
    }))

    sym.set("make-atom", Apply("make-atom", Array(Atom("Value")), (a, s) => {
      Atom(interpreter.eval(a(0), s).toString)
    }))

    sym.set("gensym", Apply("gensym", Array(), (a, s) => {
      s.gensym()
    }))

    sym.set("read-token", Apply("read-token", Array(Atom("Stream")), (args, s) => {
      val stream = interpreter.eval(args(0), s).asInstanceOf[rTokenStream]
      if (stream.atEnd) throw new RuntimeException("Unexpected end of stream")
      stream.consume() match {
        case TWord(text) => Atom(text)
      }
    }))
  }

  // Runtime core functions
  def core(): SymbolTable = {
    val sym = new SymbolTable
    sym.lex.addDelimiter('(')
    sym.lex.addDelimiter(')')
    sym.lex.addDelimiter('?')
    
    injectMacroTools(sym)

    // --- SMT Solving Utilities ---
    // Mapping from name to Z3 IntExpr
    val logicalVars = collection.mutable.Map[String, com.microsoft.z3.IntExpr]()

    def toZ3(node: Node, s: SymbolTable): com.microsoft.z3.Expr[_] = node match {
      case Atom(v) =>
        try {
          s.z3.mkInt(v.toInt)
        } catch {
          case _: Exception => 
            logicalVars.getOrElse(v, throw new RuntimeException(s"Unknown symbol in constraint: $v"))
        }
      case Apply("+", args, _, _) => s.z3.mkAdd(toZ3(args(0), s).asInstanceOf[com.microsoft.z3.ArithExpr[_]], toZ3(args(1), s).asInstanceOf[com.microsoft.z3.ArithExpr[_]])
      case Apply("-", args, _, _) => s.z3.mkSub(toZ3(args(0), s).asInstanceOf[com.microsoft.z3.ArithExpr[_]], toZ3(args(1), s).asInstanceOf[com.microsoft.z3.ArithExpr[_]])
      case Apply("*", args, _, _) => s.z3.mkMul(toZ3(args(0), s).asInstanceOf[com.microsoft.z3.ArithExpr[_]], toZ3(args(1), s).asInstanceOf[com.microsoft.z3.ArithExpr[_]])
      case Apply("==", args, _, _) => s.z3.mkEq(toZ3(args(0), s), toZ3(args(1), s))
      case Apply("<", args, _, _) => s.z3.mkLt(toZ3(args(0), s).asInstanceOf[com.microsoft.z3.ArithExpr[_]], toZ3(args(1), s).asInstanceOf[com.microsoft.z3.ArithExpr[_]])
      case Apply(">", args, _, _) => s.z3.mkGt(toZ3(args(0), s).asInstanceOf[com.microsoft.z3.ArithExpr[_]], toZ3(args(1), s).asInstanceOf[com.microsoft.z3.ArithExpr[_]])
      case Apply("?", args, _, _) => 
        val name = args(0).asInstanceOf[Atom].s
        logicalVars.getOrElseUpdate(name, s.z3.mkIntConst(name))
      case other => throw new RuntimeException(s"Unsupported node in SMT constraint: $other")
    }

    sym.set("?", Apply("?", Array(Atom("Name")), (args, s) => {
      val name = args(0) match {
        case Atom(n) => n
        case other => interpreter.eval(other, s).toString
      }
      logicalVars.getOrElseUpdate(name, s.z3.mkIntConst(name))
    }))

    sym.set("solve", Apply("solve", Array(Atom("Constraint")), (args, s) => {
      val solver = s.z3.mkSolver()
      val constraint = toZ3(args(0), s).asInstanceOf[com.microsoft.z3.BoolExpr]
      solver.add(constraint)
      
      if (solver.check() == com.microsoft.z3.Status.SATISFIABLE) {
        val model = solver.getModel()
        // Format results as a list of assignments
        model.toString
      } else {
        "UNSAT"
      }
    }))

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

    // Use ASCII code 92 for backslash to avoid string escape issues
    val backslash = 92.toChar.toString
    sym.set(backslash, Apply(backslash, Array(Atom("Arg"), Atom("Expr"), Atom("ApplyArg")), (args, s) => {
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

    // --- Macro Registration ---

    sym.set("macro-ast", Apply("macro-ast", Array(Atom("Name"), Atom("Body")), (args, s) => {
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
          }), Map("phase" -> "ast"))
        case node: Node =>
          s.set(name, Apply(name, Array(), (a, sym) => node), Map("phase" -> "ast"))
        case value =>
          s.set(name, Apply(name, Array(), (a, sym) => Atom(value.toString)), Map("phase" -> "ast"))
      }
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
          }), Map("phase" -> "comptime"))
        case value =>
          s.set(name, Apply(name, Array(), (a, sym) => value), Map("phase" -> "comptime"))
      }
      name
    }))

    val macroCharParser: CustomParser = (p, in) => {
      p.anyAtom(in) match {
        case p.Success(charArg, next1) =>
          p.expr(next1) match {
            case p.Success(bodyArg, next2) =>
              p.Success(Array[Node](charArg, bodyArg), next2)
            case ns: p.NoSuccess => ns.asInstanceOf[p.ParseResult[Array[Node]]]
          }
        case ns: p.NoSuccess => ns.asInstanceOf[p.ParseResult[Array[Node]]]
      }
    }

    sym.set("macro-char", Apply("macro-char", Array(Atom("Char"), Atom("Body")), (args, s) => {
      val charStr = args(0) match {
        case Atom(text) => text
        case other => interpreter.eval(other, s).toString
      }
      
      val char = if (charStr.nonEmpty && charStr.forall(_.isDigit)) {
        charStr.toInt.toChar
      } else if (charStr.nonEmpty) {
        charStr(0)
      } else throw new RuntimeException("macro-char requires a character or ASCII code")

      val charName = char.toString
      s.lex.addDelimiter(char)
      
      val registrationSym = s.extend()
      injectMacroTools(registrationSym)
      registrationSym.set("__macro_args__", Apply("__macro_args__", Array(), (x, y) => ()))
      val bodyResult = interpreter.eval(args(1), registrationSym)
      
      bodyResult match {
        case func: Function2[Array[Node], SymbolTable, Any] @unchecked =>
          s.set(charName, Apply(charName, Array(Atom("Args")), (a, sym) => {
            val macroSym = sym.extend()
            injectMacroTools(macroSym)
            macroSym.set("__macro_args__", Apply("__macro_args__", a, (x, y) => ()))
            func(a, macroSym)
          }), Map("phase" -> "ast"))
        case node: Node =>
          s.set(charName, Apply(charName, Array(), (a, sym) => node), Map("phase" -> "ast"))
        case value =>
          s.set(charName, Apply(charName, Array(), (a, sym) => Atom(value.toString)), Map("phase" -> "ast"))
      }
      charName
    }, Some(macroCharParser)))

    sym
  }

  // IO functions
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