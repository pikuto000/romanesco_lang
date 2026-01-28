package romanesco

import scala.collection.mutable

// =============================
// Parse Rules Implementation
// =============================

class StandardRule[A, B](
    override val name: String,
    override val pattern: Vector[Predicate[A]],
    override val build: Vector[Tree[B]] => Tree[B]
) extends ParseRule[A, B] {

  override def apply(tree: Tree[A]): Vector[Tree[Any]] = {
    // 1. Matches at root
    val rootMatches = recurse(Vector(tree), pattern, Vector.empty)

    // 2. Matches in children (deep traversal)
    val childrenMatches = tree match {
      case Tree.V(value, branches) =>
        branches.zipWithIndex.flatMap { case (child, index) =>
          // Try to apply this rule to the child
          val childResults = this.apply(child)

          // Reconstruct the parent tree with the modified child
          childResults.map { newChild =>
            // We cast to Tree[A] assuming A is Any or compatible, 
            // or effectively creating a mixed tree which logic handles as Tree[Any]
            Tree.V(value, branches.updated(index, newChild.asInstanceOf[Tree[A]]))
          }
        }
      case Tree.E() => Vector.empty
    }

    rootMatches ++ childrenMatches.asInstanceOf[Vector[Tree[Any]]]
  }

  private def recurse(
      roots: Vector[Tree[A]],
      preds: Vector[Predicate[A]],
      acc: Vector[Tree[A]]
  ): Vector[Tree[Any]] = {
    if (preds.isEmpty) {
      // Pattern matched completely
      // roots contains the continuations (branches of the last matched node)
      val result = build(acc.asInstanceOf[Vector[Tree[B]]])
      val resultWithContinuation = result match {
        case Tree.V(v, b) =>
          // Append the continuations to the branches of the built node
          Tree.V(v, b ++ roots.asInstanceOf[Vector[Tree[B]]])
        case Tree.E() =>
          // If the result is Empty, it remains Empty (or maybe we should attach? Unlikely)
          Tree.E()
      }
      Vector(resultWithContinuation.asInstanceOf[Tree[Any]])
    } else {
      val p = preds.head
      val nextPreds = preds.tail

      roots.flatMap {
        case node @ Tree.V(value, branches) =>
          if (p(value)) {
            recurse(branches, nextPreds, acc :+ node)
          } else {
            Vector.empty
          }
        case Tree.E() =>
          Vector.empty
      }
    }
  }
}

object RuleFactory {
  def create[A, B](
      name: String,
      pattern: Vector[Predicate[A]],
      builder: Vector[Tree[B]] => Tree[B]
  ): ParseRule[A, B] = {
    new StandardRule(name, pattern, builder)
  }
}

// Common Predicates
object Predicates {
  def any[A]: Predicate[A] = (_: A) => true
  
  def is[A](expected: A): Predicate[A] = (a: A) => a == expected
  
  def matches[A](f: A => Boolean): Predicate[A] = (a: A) => f(a)
}