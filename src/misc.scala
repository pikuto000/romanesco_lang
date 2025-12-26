package parser
import parser.Node
import parser.Apply
import parser.SymbolTable
import parser.interpreter

object init{
  def setup:SymbolTable={
    val sym = new SymbolTable

    // Helper for strict evaluation with Auto-Currying logic
    def strict(f: Array[Any] => Any): (Array[Node], SymbolTable) => Any = {
      def wrapper(nodes: Array[Node], s: SymbolTable): Any = {
        // Check for placeholders (_) in arguments
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
            // Recurse to check if we still have placeholders
            wrapper(mergedNodes, callerSym)
          }
        } else {
          // No placeholders: Evaluate all arguments and execute the core logic
          val args = nodes.map(interpreter.eval(_, s))
          f(args)
        }
      }
      wrapper
    }

    sym.set(
    "+",
    Apply(
    "+",
    Array(Atom("Left"),Atom("Right")),
    strict(args => {
      val left = args(0).asInstanceOf[BigDecimal]
      val right = args(1).asInstanceOf[BigDecimal]
      left + right})))
    
    sym.set(
    "-",
    Apply(
    "-",
    Array(Atom("Left"),Atom("Right")),
    strict(args => {
      val left=args(0).asInstanceOf[BigDecimal]
      val right=args(1).asInstanceOf[BigDecimal]
      left-right})))

    sym.set(
    "*",
    Apply(
    "*",
    Array(Atom("Left"),Atom("Right")),
    strict(args => {
      val left=args(0).asInstanceOf[BigDecimal]
      val right=args(1).asInstanceOf[BigDecimal]
      left*right})))
    
    sym.set(
    "/",
    Apply(
    "/",
    Array(Atom("Left"),Atom("Right")),
    strict(args => {
      val left=args(0).asInstanceOf[BigDecimal]
      val right=args(1).asInstanceOf[BigDecimal]
      left/right})))

    sym.set(
    "%",
    Apply(
    "%",
    Array(Atom("Left"),Atom("Right")),
    strict(args => {
      val left=args(0).asInstanceOf[BigDecimal]
      val right=args(1).asInstanceOf[BigDecimal]
      left%right})))

    sym.set(
    "^",
    Apply(
    "^",
    Array(Atom("Left"),Atom("Right")),
    strict(args => {
      val left=args(0).asInstanceOf[BigDecimal]
      val right=args(1).asInstanceOf[BigDecimal]
      left.pow(right.toInt)}))) // BigDecimal.pow expects an Int

    sym.set(
    "case",
    Apply(
    "case",
    Array(Atom("Cond"),Atom("Then"),Atom("Else")),
    (args, s) => {
      val cond = interpreter.eval(args(0), s).asInstanceOf[Boolean]
      if (cond) interpreter.eval(args(1), s) else interpreter.eval(args(2), s)}))
    
    sym.set(
    "==",
    Apply(
    "==",
    Array(Atom("Left"),Atom("Right")),
    strict(args => {
      val left=args(0)
      val right=args(1)
      left == right})))

    sym.set(
    "!=",
    Apply(
    "!=",
    Array(Atom("Left"),Atom("Right")),
    strict(args => {
      val left=args(0)
      val right=args(1)
      left != right})))

    sym.set(
    "<",
    Apply(
    "<",
    Array(Atom("Left"),Atom("Right")),
    strict(args => {
      val left=args(0).asInstanceOf[BigDecimal]
      val right=args(1).asInstanceOf[BigDecimal]
      left < right})))

    sym.set(
    ">",
    Apply(
    ">",
    Array(Atom("Left"),Atom("Right")),
    strict(args => {
      val left=args(0).asInstanceOf[BigDecimal]
      val right=args(1).asInstanceOf[BigDecimal]
      left > right})))

    sym.set(
    "=<",
    Apply(
    "<=",
    Array(Atom("Left"),Atom("Right")),
    strict(args => {
      val left=args(0).asInstanceOf[BigDecimal]
      val right=args(1).asInstanceOf[BigDecimal]
      left <= right})))

    sym.set(
    ">=",
    Apply(
    ">=",
    Array(Atom("Left"),Atom("Right")),
    strict(args => {
      val left=args(0).asInstanceOf[BigDecimal]
      val right=args(1).asInstanceOf[BigDecimal]
      left >= right})))

    sym.set(
    ":-",
    Apply(
    ":-",
    Array(Atom("Name"),Atom("Constraint/Type"),Atom("Expr")),
    (args, s) => {
      val name = args(0).asInstanceOf[Atom].s
      val ConstraintType = args(1)
      val exprNode = args(2)
      
      // Evaluate expression to get the function/value
      val expr = interpreter.eval(exprNode, s)

      val funcOpt = expr match {
        // Functions now use (Array[Node], SymbolTable) => Any
        case f: Function2[_,_,_] => Some(f.asInstanceOf[(Array[Node], SymbolTable) => Any])
        case _ => None
      }

      if (funcOpt.isDefined) {
        // Register as a 1-argument operator. Use the original name.
        s.set(name, Apply(name, Array(Atom("Arg")), funcOpt.get))
      } else {
        // Register as a 0-argument operator (variable).
        // The thunk just returns the evaluated 'expr'
        s.set(name, Apply(name, Array(), (nodes, s2) => expr))
      }
      expr
      }))

    sym.set(
    //ラムダ抽象
    "\\",
    Apply(
    "\\",
    Array(Atom("Arg"),Atom("Expr"),Atom("ApplyArg")),
    (args, s) => {
      val argNode = args(0)
      val body = args(1)
      val applyArg = args(2)
      
      val argName = argNode match {
        case Atom(name) => name
        case _ => throw new IllegalArgumentException(s"Lambda argument must be an Atom, got $argNode")
      }

      // Capture environment for closure
      val capturedSym = s

      val lambdaFunc: (Array[Node], SymbolTable) => Any = (lambdaNodes, callerSym) => {
          if (lambdaNodes.length != 1) {
             throw new IllegalArgumentException(s"Lambda expects 1 argument, but got ${lambdaNodes.length}")
          }
          
          // Evaluate argument in caller's scope
          val argVal = interpreter.eval(lambdaNodes(0), callerSym)
          
          // New scope extending definition scope
          val lambdaSym = new SymbolTable
          capturedSym.getTabKeys.foreach(key => lambdaSym.set(key, capturedSym.look(key)))
          
          // Bind argument using a thunk to preserve value/type
          val valueWrapper = Apply("val", Array(), (nodes, s) => argVal)
          lambdaSym.set(argName, valueWrapper)
          
          interpreter.eval(body, lambdaSym)
      }
      
      applyArg match {
        case Atom("_") => lambdaFunc
        case _ => 
          // Immediate application
          // Call lambdaFunc with applyArg node and current scope
          lambdaFunc(Array(applyArg), s)
      }
    }))

    // Structural Application (Juxtaposition)
    sym.set(
      "",
      Apply(
        "",
        Array(Atom("Func"), Atom("Arg")),
        (args, s) => {
          val funcVal = interpreter.eval(args(0), s)
          val func = funcVal match {
            case f: Function2[_,_,_] => f.asInstanceOf[(Array[Node], SymbolTable) => Any]
            case _ => throw new IllegalArgumentException(s"Expected a function in structural application, got $funcVal")
          }
          func(Array(args(1)), s)
        }
      )
    )

    sym.set(
    "write",
    Apply(
    "write",
    Array(Atom("Expr")),
    strict(args => {
      val expr = args(0)
      println(expr)
      expr})))

    sym.set(
    "abort",
    Apply(
    "abort",
    Array(),
    strict(args => {
      println("Aborted.")
      sys.exit(1)})))

    sym
  }
}

/*
* 演算子の定義:
+ Left Right
- Left Right
* Left Right
/ Left Right
% Left Right
^ Left Right
case Cond Then Else
== Left Right
!= Left Right
< Left Right
> Left Right
<= Left Right
>= Left Right
:- Name Constriant/Type Expr
\ arg Expr
write Expr
abort
*/