package romanesco
//インタプリタを実装する
import scala.collection.mutable

trait Interpreter {
  def eval(tree: Tree[Any]): Any
}

class SolverInterpreter(registry: Registory) extends Interpreter {

  override def eval(tree: Tree[Any]): Any = {
    val prop = treeToProp(tree)
    println(s"Interpreting Goal: $prop")
    Prover.prove(prop) match {
      case Some(term) =>
        println(s"Proof Found: $term")
        term
      case None =>
        println("No proof found.")
        null
    }
  }

  private def treeToProp(tree: Tree[Any]): Prop = {
    tree match {
      case Tree.V("Prop", children)  => treeToProp(children.head)
      case Tree.V("Arrow", children) =>
        Prop.Arrow(treeToProp(children(0)), treeToProp(children(1)))
      case Tree.V("And", children) =>
        Prop.And(treeToProp(children(0)), treeToProp(children(1)))
      case Tree.V("Or", children) =>
        Prop.Or(treeToProp(children(0)), treeToProp(children(1)))
      case Tree.V("Forall", children) =>
        val varName = extractString(children(0))
        Prop.Forall(varName, treeToProp(children(1)))
      case Tree.V("Exists", children) =>
        val varName = extractString(children(0))
        Prop.Exists(varName, treeToProp(children(1)))
      case Tree.V("Eq", children) =>
        Prop.Eq(treeToInd(children(0)), treeToInd(children(1)))
      case Tree.V("Pred", children) =>
        val name = extractString(children(0))
        val args = if (children.size > 1) {
          children.tail.map(treeToInd).toList
        } else Nil
        Prop.Pred(name, args)
      case Tree.V("True", _)  => Prop.True
      case Tree.V("False", _) => Prop.False
      case _                  =>
        throw new RuntimeException(s"Unexpected tree node for Prop: $tree")
    }
  }

  private def treeToInd(tree: Tree[Any]): Ind = {
    tree match {
      case Tree.V("Ind", children)   => treeToInd(children.head)
      case Tree.V("Var", children)   => Ind.Var(extractString(children(0)))
      case Tree.V("Const", children) => Ind.Const(extractString(children(0)))
      case Tree.V("Meta", children)  =>
        Ind.Meta(extractString(children(0)).toInt)
      case _ => Ind.Const(extractString(tree))
    }
  }

  private def extractString(tree: Tree[Any]): String = {
    tree match {
      case Tree.V((_, _, _, content: String), _)    => content
      case Tree.V(_, children) if children.nonEmpty =>
        extractString(children.head)
      case _ => tree.toString
    }
  }
}

//testInterpreter
@main def testInterpreter: Unit = {
  Debug.logger.switch(true)
  val reg = new Registory()
  val interpreter = new SolverInterpreter(reg)
  reg.pushInterpreter(interpreter)
  // Manual construction of a Tree representing: forall x. P(x) -> P(x)
  // Structure: Forall("x", Arrow(Pred("P", Ind("x")), Pred("P", Ind("x"))))

  val tokenX = (0, 0, "".r, "x")
  val tokenP = (0, 0, "".r, "P")

  val pX: Tree[Any] = Tree.V(
    "Pred",
    Vector(
      Tree.V(tokenP, Vector.empty),
      Tree.V("Var", Vector(Tree.V(tokenX, Vector.empty)))
    )
  )

  val arrowNode: Tree[Any] = Tree.V("Arrow", Vector(pX, pX))

  val forallTree: Tree[Any] = Tree.V(
    "Forall",
    Vector(
      Tree.V(tokenX, Vector.empty),
      arrowNode
    )
  )

  println("--- Running Interpreter Test ---")
  val result = reg.currentInterpreter.eval(forallTree)
  println(s"Final Result: $result")
}
