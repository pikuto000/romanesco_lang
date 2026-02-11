package romanesco.Solver.SolverTests

import romanesco.Solver.core._
import romanesco.Solver.core.Expr._
import romanesco.Solver.core.LogicSymbols._
import romanesco.Solver.TestParser
import romanesco.Utils.Debug.logger

object RecursiveInductionTest {
  def main(args: Array[String]): Unit = {
    logger.switch(false)
    val rules = StandardRules.all
    val config = ProverConfig(rules = rules, maxInduction = 2)
    val prover = new Prover(config)

    println("=== Recursive Data Structure Induction Test ===")

    val testCases = List(
      "∀xs. append(xs, nil) = xs",
      "∀xs. ∀ys. ∀zs. append(append(xs, ys), zs) = append(xs, append(ys, zs))",
      "∀t. mirror(mirror(t)) = t",
      "∀xs. ∀ys. list_prop(append(xs, ys))" // This will require induction on xs in context if we had a hypothesis about list_prop
    )

    // Add list/tree rules
    val append_nil = CatRule("append_nil", sym("append")(sym("nil"), v("ys")), v("ys"))
    val append_cons = CatRule("append_cons", sym("append")(sym("cons")(v("x"), v("xs")), v("ys")), sym("cons")(v("x"), sym("append")(v("xs"), v("ys"))))
    
    val mirror_leaf = CatRule("mirror_leaf", sym("mirror")(sym("leaf")), sym("leaf"))
    val mirror_node = CatRule("mirror_node", sym("mirror")(sym("node")(v("l"), v("v"), v("r"))), sym("node")(sym("mirror")(v("r")), v("v"), sym("mirror")(v("l"))))

    val prop_nil = CatRule("prop_nil", sym("list_prop")(v("ys")), sym(True)) // dummy rule
    
    val allRules = List(append_nil, append_cons, mirror_leaf, mirror_node, prop_nil)
    val extendedProver = new Prover(config.copy(rules = rules ++ allRules))

    testCases.foreach { input =>
      print(s"Case: $input ... ")
      val goal = TestParser.parse(input)
      extendedProver.prove(goal, maxDepth = 15) match {
        case Right(res) => 
          println("✓ OK")
          // Check if induction was used
          if (res.tree.format().contains("induction[")) println("  (Induction used)")
        case Left(trace) => 
          println("✗ FAIL")
          // println(trace.format())
      }
    }
  }
}
