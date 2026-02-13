package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object DebugDeep {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    logger.setMaxDepth(10)

    val monadRules = List(
      CatRule(
        "return_list",
        Expr.App(Expr.Sym("return"), List(Expr.Var("x"))),
        Expr.App(Expr.Sym("cons"), List(Expr.Var("x"), Expr.Sym("nil"))),
        List(Expr.Var("x"))
      ),

      CatRule(
        "bind_nil",
        Expr.App(Expr.Sym("bind"), List(Expr.Sym("nil"), Expr.Var("f"))),
        Expr.Sym("nil"),
        List(Expr.Var("f"))
      ),

      CatRule(
        "bind_cons",
        Expr.App(
          Expr.Sym("bind"),
          List(
            Expr.App(Expr.Sym("cons"), List(Expr.Var("x"), Expr.Var("xs"))),
            Expr.Var("f")
          )
        ),
        Expr.App(
          Expr.Sym("append"),
          List(
            Expr.App(Expr.Var("f"), List(Expr.Var("x"))),
            Expr.App(Expr.Sym("bind"), List(Expr.Var("xs"), Expr.Var("f")))
          )
        ),
        List(Expr.Var("x"), Expr.Var("xs"), Expr.Var("f"))
      )
    )

    val config = ProverConfig(
      rules = monadRules ++ StandardRules.listAppendRules ++ StandardRules.all,
      algebras = List(StandardRules.listAlgebra)
    )

    val prover = new Prover(config)

    val input = "∀m. bind(m, λx. return(x)) = m"
    println(s"Proving: $input")
    val goal = TestParser.parse(input)
    prover.prove(goal, maxDepth = 15, timeoutMs = 30000) match {
      case Right(result) =>
        println("✁ESolved!")
        println(result.tree.format(0))
      case Left(trace) =>
        println(s"✁EFailed. reason: ${trace.reason}")
      // println(trace.format(0))
    }
  }
}
