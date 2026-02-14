package romanesco.Types
enum Tree[T] {
  case E() // empty
  case V(value: T, branch: Vector[Tree[T]])

  def flatten: Vector[Vector[T]] = {
    this match {
      case E()                  => Vector.empty
      case V(v, b) if b.isEmpty => Vector(Vector(v))
      case V(v, b)              => b.flatMap(_.flatten).map(v +: _)
    }
  }

  // for debugging
  def prettyPrint(
      prefix: String = "",
      isLast: Boolean = true,
      isRoot: Boolean = true
  ): String = {
    val marker = if (isRoot) "" else if (isLast) "+-- " else "|-- "
    val currentLine = s"$prefix$marker$valueString\n"

    val nextPrefix =
      if (isRoot) "" else prefix + (if (isLast) "    " else "|   ")
    val branches = this match {
      case E()     => Vector.empty
      case V(_, b) => b
    }

    val children = branches.zipWithIndex.map { case (child, i) =>
      child.prettyPrint(nextPrefix, i == branches.size - 1, false)
    }.mkString

    currentLine + children
  }

  private def valueString: String = this match {
    case E()     => "E"
    case V(v, _) => v.toString.replace("\n", "\\n").replace("\r", "\\r")
  }

  // for debugging
  override def toString: String = {
    this.match {
      case E()     => "E"
      case V(v, b) => {
        s"V($v, ${b.map(_.toString).mkString("[", ",", "]")})"
          .replace("\n", "\\n")
          .replace("\r", "\\r")
      }
    }
  }

  def toJson(f: T => String): String = this match {
    case E() => "null"
    case V(v, b) => 
      s"{\"value\":${f(v)},\"branches\":[${b.map(_.toJson(f)).mkString(",")}]}"
  }
}

object Tree {
  def merge[T](trees: Vector[Tree[T]]): Vector[Tree[T]] = {
    val vs = trees.collect { case v: Tree.V[T] => v }
    val merged = vs
      .groupBy(_.value)
      .map { case (value, group) =>
        Tree.V(value, merge(group.flatMap(_.branch)))
      }
      .toVector

    val hasEmpty = trees.exists { case Tree.E() => true; case _ => false }
    if (hasEmpty) merged :+ Tree.E() else merged
  }

  // inverse of flatten method
  def fromPaths[T](paths: Vector[Vector[T]]): Vector[Tree[T]] = {
    paths
      .filter(_.nonEmpty)
      .groupBy(_.head)
      .map { case (value, subPaths) =>
        Tree.V(value, fromPaths(subPaths.map(_.tail)))
      }
      .toVector
  }

  // just alias of fromPaths
  def unflatten[T](vecs: Vector[Vector[T]]): Vector[Tree[T]] = fromPaths(vecs)
}

type UInt = Int
object Uint {
  import scala.compiletime.error
  inline def apply(inline n: Int): UInt = {
    inline if (n >= 0) n else error("UInt cannot be negative")
  }
}

type OpTree = Tree[String]
type Programs = Vector[OpTree]
