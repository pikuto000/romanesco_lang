package parser
import parser.Node
import parser.Apply
import parser.SymbolTable
import parser.interpreter

object init{
  def setup:SymbolTable={
    val sym = new SymbolTable

    // Helper for strict evaluation (standard operators)
    def strict(f: Array[Any] => Any): (Array[Node], SymbolTable) => Any = {
      (nodes, s) => {
        val args = nodes.map(interpreter.eval(_, s))
        f(args)
      }
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
        // Since we now use (Array[Node], SymbolTable) => Any, check for that type
        case f: Function2[_,_,_] => Some(f.asInstanceOf[(Array[Node], SymbolTable) => Any])
        case _ => None
      }

      if (funcOpt.isDefined) {
        sym.set(name, Apply(name, Array(Atom("Arg")), funcOpt.get))
      } else {
        // Value wrapper (Thunk)
        // Wraps 'expr' value. When called, it returns 'expr' value.
        sym.set(name, Apply(name, Array(), (nodes, s) => expr))
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

      // Capture current environment (lexical scoping)
      val capturedSym = s

      // The lambda function itself
      val lambdaFunc: (Array[Node], SymbolTable) => Any = (lambdaArgs, _) => {
         // Note: We ignore the SymbolTable passed at call time (except maybe for some dynamic scoping if needed, but for lexical we use capturedSym)
         // Actually, typically we might want to chain them or use capturedSym.
         // In standard closure, we extend capturedSym.
         
         if (lambdaArgs.length != 1) {
             throw new IllegalArgumentException(s"Lambda expects 1 argument, but got ${lambdaArgs.length}")
         }

         val lambdaSym = new SymbolTable
         // Copy captured environment
         capturedSym.getTabKeys.foreach(key => lambdaSym.set(key, capturedSym.look(key)))

         // Evaluate argument (strict evaluation for now, or we can make it lazy if we pass Node)
         // Since this is called by Apply logic which might pass raw Nodes if we want lazy... 
         // But strict() helper evaluates.
         // Here, lambdaFunc is called by 'eval' or 'applyArg' logic.
         // If called by 'eval' (via Apply node), 'lambdaArgs' are NODES.
         // So we should evaluate them here if we want strict, or wrap them if we want lazy.
         // Let's stick to strict for arguments for now.
         val argValue = interpreter.eval(lambdaArgs(0), capturedSym) // Evaluate in caller's scope? Or captured? Caller's scope usually.
         // Wait. The `lambdaArgs` are Nodes passed from the call site.
         // The call site context is passed as `_` (ignored above).
         // We should use the CALL SITE symbol table to evaluate arguments!
         
         // Rethink:
         // val lambdaFunc: (Array[Node], SymbolTable) => Any = (lambdaArgs, callerSym) => ...
         // val argValue = interpreter.eval(lambdaArgs(0), callerSym)
         
         // But wait! If we use `strict` helper for OTHER functions, they evaluate args using `s` (call site sym).
         // So `lambdaFunc` receives `callerSym`.
      }
      
      // Let's refine lambdaFunc
      val finalLambdaFunc: (Array[Node], SymbolTable) => Any = (lambdaNodes, callerSym) => {
          if (lambdaNodes.length != 1) {
             throw new IllegalArgumentException(s"Lambda expects 1 argument, but got ${lambdaNodes.length}")
          }
          
          // Evaluate argument in CALLER scope
          val argVal = interpreter.eval(lambdaNodes(0), callerSym)
          
          // Create new scope extending CAPTURED scope
          val lambdaSym = new SymbolTable
          capturedSym.getTabKeys.foreach(key => lambdaSym.set(key, capturedSym.look(key)))
          
          // Bind argument
          // Use Thunk wrapper to preserve type
          val valueWrapper = Apply("val", Array(), (nodes, s) => argVal)
          lambdaSym.set(argName, valueWrapper)
          
          // Evaluate body in LAMBDA scope
          interpreter.eval(body, lambdaSym)
      }

      applyArg match {
        case Atom("_") => finalLambdaFunc
        case _ => 
          // Immediate application
          // We treat `applyArg` as the argument node.
          // We call finalLambdaFunc with this node and the CURRENT scope `s`.
          finalLambdaFunc(Array(applyArg), s)
      }
    }))

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