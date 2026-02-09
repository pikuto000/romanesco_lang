package romanesco.Solver

import romanesco.Solver.core._
import romanesco.Solver.sugar._
import romanesco.Utils.Debug.logger

@main def testPatternUnification = {
  logger.switch(true)
  println("=== Pattern Unification Test ===")

  // Test 1: ?P(x) = x = 0
  // Expected: ?P -> λx. x = 0
  println("Test 1: ?P(x) = x = 0")
  val metaP = Expr.Meta(MetaId(List(1)))
  val x = Expr.Var("x")
  val zero = Expr.Sym("0")
  val eq = Expr.Sym("=")
  
  // LHS: ?P(x)
  val lhs = Expr.App(metaP, List(x))
  // RHS: x = 0
  val rhs = Expr.App(eq, List(x, zero))

  println(s"Unifying: $lhs = $rhs")
  val results = Unifier.unify(lhs, rhs, Unifier.emptySubst)
  
  if (results.nonEmpty) {
    println("✓ Solutions found:")
    results.zipWithIndex.foreach { case (subst, i) =>
      println(s"  Solution $i:")
      subst.foreach { case (id, expr) =>
        println(s"    ?$id -> $expr")
      }
      // Check if substitution works
      val appliedLhs = Unifier.applySubst(lhs, subst)
      val normalized = Rewriter.normalize(appliedLhs)
      println(s"    Applied LHS: $appliedLhs")
      println(s"    Normalized:  $normalized")
      if (normalized == rhs) println("    ✓ Valid solution")
      else println("    ✗ Invalid solution")
    }
  } else {
    println("✗ No solution found")
  }

  // Test 2: ?M(x) = S(x)
  println("\nTest 2: ?M(x) = S(x)")
  val s = Expr.Sym("S")
  val metaM = Expr.Meta(MetaId(List(2)))
  val lhs2 = Expr.App(metaM, List(x))
  val rhs2 = Expr.App(s, List(x))

  println(s"Unifying: $lhs2 = $rhs2")
  val results2 = Unifier.unify(lhs2, rhs2, Unifier.emptySubst)
  if (results2.nonEmpty) {
    results2.headOption.foreach { subst =>
      println(s"  Solution: ${subst(MetaId(List(2)))}")
      // Expected: λx. S(x)
    }
  } else {
    println("✗ No solution found")
  }
}