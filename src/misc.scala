package parser
import parser.Node
import parser.Apply
import parser.SymbolTable
import parser.interpreter

object init {
  def strict(f: Array[Any] => Any): (Array[Node], SymbolTable) => Any = {
    def wrapper(nodes: Array[Node], s: SymbolTable): Any = {
      val hasPlaceholder = nodes.exists { case Atom("_") => true case _ => false }
      if (hasPlaceholder) {
        (extraNodes: Array[Node], callerSym: SymbolTable) => {
          var extraIdx = 0
          val mergedNodes = nodes.map {
            case Atom("_") if extraIdx < extraNodes.length =>
              val n = extraNodes(extraIdx); extraIdx += 1; n
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

  def injectMacroTools(sym: SymbolTable): Unit = {
    sym.set("get-env", Apply("get-env", Array(), (a, s) => s))
    sym.set("get-macro-env", Apply("get-macro-env", Array(), (a, s) => {
      s.get("__macro_env__") match {
        case Some(resolved) => interpreter.eval(resolved, s)
        case _ => throw new RuntimeException("get-macro-env error")
      }
    }))
    sym.set("eval-in", Apply("eval-in", Array(Atom("Env"), Atom("Node")), (args, s) => {
      val targetEnv = interpreter.eval(args(0), s).asInstanceOf[SymbolTable]
      val node = interpreter.eval(args(1), s).asInstanceOf[Node]
      interpreter.eval(node, targetEnv)
    }))
    sym.set("set-in", Apply("set-in", Array(Atom("Env"), Atom("Name"), Atom("Val")), (args, s) => {
      val targetEnv = interpreter.eval(args(0), s).asInstanceOf[SymbolTable]
      val name = interpreter.eval(args(1), s) match {
        case n: Node => n.rawName
        case s: String => s
        case other => other.toString
      }
      val value = interpreter.eval(args(2), s) match { case n: Node => n case other => Atom(other.toString) }
      targetEnv.set(name, value)
      value
    }))
    sym.set("get-arg", Apply("get-arg", Array(Atom("Index")), (args, s) => {
      s.get("__macro_args__") match {
        case Some(Apply(_, nodes, _, _)) if nodes.nonEmpty => 
          val idx = interpreter.eval(args(0), s).asInstanceOf[BigDecimal].toInt
          nodes(idx)
        case _ => throw new RuntimeException("get-arg error")
      }
    }))
    sym.set("make-apply", Apply("make-apply", Array(Atom("Fun"), Atom("Args")), (args, s) => {
      val funName = interpreter.eval(args(0), s) match {
        case n: Node => n.rawName
        case s: String => s
        case other => other.toString
      }
      val argsVal = interpreter.eval(args(1), s)
      val nodes: Array[Node] = argsVal match {
        case a: Array[Node] => a
        case Apply("cons", _, _, _) => 
          def collect(curr: Node): List[Node] = curr match {
            case a: Apply if a.fun == "cons" => a.args(0) :: collect(a.args(1))
            case _ => Nil
          }
          collect(argsVal.asInstanceOf[Node]).toArray
        case n: Node => Array(n)
        case other => Array(Atom(other.toString))
      }
      Apply(funName, nodes, s.getFunc(funName))
    }))
    sym.set("make-atom", Apply("make-atom", Array(Atom("Value")), (a, s) => Atom(interpreter.eval(a(0), s).toString)))
    sym.set("gensym", Apply("gensym", Array(), (a, s) => s.gensym()))
    sym.set("read-token", Apply("read-token", Array(Atom("Stream")), (args, s) => {
      val stream = interpreter.eval(args(0), s).asInstanceOf[rTokenStream]
      if (stream.atEnd) throw new RuntimeException("Unexpected end of stream")
      stream.consume() match { case TWord(text) => Atom(text) }
    }))
  }

  def core(): SymbolTable = {
    val sym = new SymbolTable
    sym.lex.addDelimiter('('); sym.lex.addDelimiter(')'); sym.lex.addDelimiter('?')
    sym.lex.addDelimiter('&'); sym.lex.addDelimiter('|'); sym.lex.addDelimiter('!')
    sym.lex.addDelimiter('='); sym.lex.addDelimiter('>'); sym.lex.addDelimiter('<')
    injectMacroTools(sym)

    sym.set("nil", Apply("nil", Array(), (args, s) => Atom("nil")))
    sym.set("cons", Apply("cons", Array(Atom("H"), Atom("T")), (args, s) => Apply("cons", args, sym.getFunc("cons"))))
    sym.set("head", Apply("head", Array(Atom("L")), (args, s) => {
      interpreter.eval(args(0), s) match { case a: Apply if a.fun == "cons" => a.args(0) case other => throw new RuntimeException(s"head error") }
    }))
    sym.set("tail", Apply("tail", Array(Atom("L")), (args, s) => {
      interpreter.eval(args(0), s) match { case a: Apply if a.fun == "cons" => a.args(1) case other => throw new RuntimeException(s"tail error") }
    }))

    sym.set("+", Apply("+", Array(Atom("L"), Atom("R")), strict(args => args(0).asInstanceOf[BigDecimal] + args(1).asInstanceOf[BigDecimal])), Map("smt-op" -> "add"))
    sym.set("-", Apply("-", Array(Atom("L"), Atom("R")), strict(args => args(0).asInstanceOf[BigDecimal] - args(1).asInstanceOf[BigDecimal])), Map("smt-op" -> "sub"))
    sym.set("*", Apply("*", Array(Atom("L"), Atom("R")), strict(args => args(0).asInstanceOf[BigDecimal] * args(1).asInstanceOf[BigDecimal])), Map("smt-op" -> "mul"))
    sym.set("/", Apply("/", Array(Atom("L"), Atom("R")), strict(args => args(0).asInstanceOf[BigDecimal] / args(1).asInstanceOf[BigDecimal])))
    sym.set("%", Apply("%", Array(Atom("L"), Atom("R")), strict(args => args(0).asInstanceOf[BigDecimal] % args(1).asInstanceOf[BigDecimal])))

    sym.set("=", Apply("=", Array(Atom("L"), Atom("R")), strict(args => args(0) == args(1))), Map("smt-op" -> "eq"))
    sym.set("&", Apply("&", Array(Atom("A"), Atom("B")), strict(args => args(0).asInstanceOf[Boolean] && args(1).asInstanceOf[Boolean])), Map("smt-op" -> "and"))
    sym.set("|", Apply("|", Array(Atom("A"), Atom("B")), strict(args => args(0).asInstanceOf[Boolean] || args(1).asInstanceOf[Boolean])), Map("smt-op" -> "or"))
    sym.set("!", Apply("!", Array(Atom("A")), strict(args => !args(0).asInstanceOf[Boolean])), Map("smt-op" -> "not"))
    sym.set(">", Apply(">", Array(Atom("L"), Atom("R")), strict(args => args(0).asInstanceOf[BigDecimal] > args(1).asInstanceOf[BigDecimal])), Map("smt-op" -> "gt"))
    sym.set("<", Apply("<", Array(Atom("L"), Atom("R")), strict(args => args(0).asInstanceOf[BigDecimal] < args(1).asInstanceOf[BigDecimal])), Map("smt-op" -> "lt"))

    sym.set("case", Apply("case", Array(Atom("C"), Atom("T"), Atom("E")), (args, s) => {
      if (interpreter.eval(args(0), s).asInstanceOf[Boolean]) interpreter.eval(args(1), s) else interpreter.eval(args(2), s)
    }))

    sym.set(":-", Apply(":-", Array(Atom("N"), Atom("C"), Atom("E")), (args, s) => {
      val name = interpreter.eval(args(0), s) match { case n: Node => n.rawName case other => other.toString }
      val expr = interpreter.eval(args(2), s)
      expr match {
        case f: Function2[?, ?, ?] => s.set(name, Apply(name, Array(Atom("Arg")), f.asInstanceOf[(Array[Node], SymbolTable) => Any]))
        case _ => s.set(name, Apply(name, Array(), (nodes, s2) => expr))
      }
      expr
    }))

    val bs = 92.toChar.toString
    sym.set(bs, Apply(bs, Array(Atom("A"), Atom("E"), Atom("X")), (args, s) => {
      val argName = args(0) match { case n: Node => n.rawName case other => other.toString }
      val body = args(1); val applyArg = args(2); val capturedSym = s
      val lambdaFunc: (Array[Node], SymbolTable) => Any = (nodes, callerSym) => {
        val argVal = interpreter.eval(nodes(0), callerSym)
        val lSym = capturedSym.extend()
        lSym.set(argName, Apply("val", Array(), (n, sy) => argVal))
        interpreter.eval(body, lSym)
      }
      if (applyArg == Atom("_")) lambdaFunc else lambdaFunc(Array(applyArg), s)
    }))

    sym.set("?", Apply("?", Array(Atom("N")), (args, s) => {
      val name = interpreter.eval(args(0), s) match { case n: Node => n.rawName case other => other.toString }
      RomanescoSolver.getOrCreateVar(name, s)
    }))
    sym.set("solve", Apply("solve", Array(Atom("C")), (args, s) => RomanescoSolver.solve(args(0), s)))

    sym.set("macro-ast", Apply("macro-ast", Array(Atom("N"), Atom("B")), (args, s) => {
      val name = interpreter.eval(args(0), s) match { case n: Node => n.rawName case other => other.toString }
      val bodyNode = args(1)
      s.set(name, Apply(name, Array(Atom("Args")), (a, callerSym) => {
        val macroSym = callerSym.extend()
        injectMacroTools(macroSym)
        macroSym.set("__macro_env__", Apply("env", Array(), (n, s) => callerSym))
        macroSym.set("__macro_args__", Apply("__macro_args__", a, (x, y) => ()))
        val macroFunc = interpreter.eval(bodyNode, macroSym).asInstanceOf[(Array[Node], SymbolTable) => Any]
        macroFunc(a, macroSym)
      }), Map("phase" -> "ast"))
      name
    }))

    sym.set("comptime", Apply("comptime", Array(Atom("N"), Atom("B")), (args, s) => {
      val name = interpreter.eval(args(0), s) match { case n: Node => n.rawName case other => other.toString }
      val bodyNode = args(1)
      s.set(name, Apply(name, Array(Atom("Args")), (a, callerSym) => {
        val macroSym = callerSym.extend(); injectMacroTools(macroSym)
        macroSym.set("__macro_env__", Apply("env", Array(), (n, s) => callerSym))
        macroSym.set("__macro_args__", Apply("__macro_args__", a, (x, y) => ()))
        val macroFunc = interpreter.eval(bodyNode, macroSym).asInstanceOf[(Array[Node], SymbolTable) => Any]
        macroFunc(a, macroSym)
      }), Map("phase" -> "comptime"))
      name
    }))

    val macroCharParser: CustomParser = (p, in) => {
      p.anyAtom(in) match {
        case p.Success(c, next1) => p.expr(next1) match {
          case p.Success(b, next2) => p.Success(Array[Node](c, b), next2)
          case ns: p.NoSuccess => ns.asInstanceOf[p.ParseResult[Array[Node]]]
        }
        case ns: p.NoSuccess => ns.asInstanceOf[p.ParseResult[Array[Node]]]
      }
    }

    sym.set("macro-char", Apply("macro-char", Array(Atom("C"), Atom("B")), (args, s) => {
      val charStr = interpreter.eval(args(0), s) match { case n: Node => n.rawName case other => other.toString }
      val char = if (charStr.nonEmpty && charStr.forall(_.isDigit)) charStr.toInt.toChar else charStr(0)
      val bodyNode = args(1)
      s.lex.addDelimiter(char)
      s.set(char.toString, Apply(char.toString, Array(Atom("Args")), (a, callerSym) => {
        val macroSym = callerSym.extend(); injectMacroTools(macroSym)
        macroSym.set("__macro_env__", Apply("env", Array(), (n, s) => callerSym))
        macroSym.set("__macro_args__", Apply("__macro_args__", a, (x, y) => ()))
        val macroFunc = interpreter.eval(bodyNode, macroSym).asInstanceOf[(Array[Node], SymbolTable) => Any]
        macroFunc(a, macroSym)
      }), Map("phase" -> "ast"))
      char.toString
    }, Some(macroCharParser)))

    sym
  }

  def registerIO(sym: SymbolTable): Unit = {
    def strict(f: Array[Any] => Any): (Array[Node], SymbolTable) => Any = { (nodes, s) => f(nodes.map(interpreter.eval(_, s))) }
    sym.set("write", Apply("write", Array(Atom("E")), strict(args => { println(args(0)); args(0) })))
    sym.set("abort", Apply("abort", Array(), strict(args => { println("Aborted."); sys.exit(1) })))
  }

  def setup: SymbolTable = { val sym = core(); registerIO(sym); sym }
}
