package romanesco.Solver

import romanesco.Solver.core._
import romanesco.Solver.sugar._
import romanesco.Utils.Debug.logger

@main def testGeneralizedUnification = {
  logger.switch(true)
  println("=== Generalized Higher-Order Unification Test ===")

  val metaP = Expr.Meta(MetaId(List(1)))
  val x = Expr.Var("x")
  val a = Expr.Sym("a")
  val b = Expr.Sym("b")
  val f = Expr.Sym("f")
  val g = Expr.Sym("g")
  val s = Expr.Sym("S")

  // Case 1: Non-variable argument abstraction
  // ?P(f(x)) = g(f(x))
  // Expected: ?P -> λu. g(u)
  println("\nTest 1: ?P(f(x)) = g(f(x))")
  val lhs1 = Expr.App(metaP, List(Expr.App(f, List(x))))
  val rhs1 = Expr.App(g, List(Expr.App(f, List(x))))
  
  println(s"Unifying: $lhs1 = $rhs1")
  val res1 = Unifier.unify(lhs1, rhs1, Unifier.emptySubst)
  printResult(res1, lhs1, rhs1)

  // Case 2: Constants as arguments
  // ?P(a, b) = f(b, a)
  // Expected: ?P -> λu v. f(v, u)
  println("\nTest 2: ?P(a, b) = f(b, a)")
  val lhs2 = Expr.App(metaP, List(a, b))
  val rhs2 = Expr.App(f, List(b, a))

  println(s"Unifying: $lhs2 = $rhs2")
  val res2 = Unifier.unify(lhs2, rhs2, Unifier.emptySubst)
  printResult(res2, lhs2, rhs2)

  // Case 3: Overlapping arguments (Ambiguous/Greedy)
  // ?P(S(x), x) = S(S(x))
  // If we abstract S(x) as u, and x as v.
  // Target S(S(x)) can be S(u).
  // Expected: ?P -> λu v. S(u)  (or λu v. S(S(v)))
  println("\nTest 3: ?P(S(x), x) = S(S(x))")
  val lhs3 = Expr.App(metaP, List(Expr.App(s, List(x)), x))
  val rhs3 = Expr.App(s, List(Expr.App(s, List(x))))

  println(s"Unifying: $lhs3 = $rhs3")
  val res3 = Unifier.unify(lhs3, rhs3, Unifier.emptySubst)
  printResult(res3, lhs3, rhs3)
}

def printResult(results: LazyList[Unifier.Subst], lhs: Expr, rhs: Expr) = {
  if (results.nonEmpty) {
    println("✓ Solutions found:")
    results.zipWithIndex.foreach { case (subst, i) =>
      println(s"  Solution $i:")
      subst.foreach { case (id, expr) =>
        println(s"    ?$id -> $expr")
      }
      val appliedLhs = Unifier.applySubst(lhs, subst)
      val normalized = Rewriter.normalize(appliedLhs)
      println(s"    Check: $normalized == $rhs")
      if (normalized == rhs) println("    ✓ Valid") else println("    ✗ Invalid")
    }
  } else {
    println("✗ No solution found")
  }
}