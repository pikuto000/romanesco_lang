package romanesco
enum Tree[T]{
  case E()//empty
  case V(value:T,branch:Vector[Tree[T]])

  def flatten:Vector[Vector[T]]={
    this match {
      case E() => Vector.empty
      case V(v, b) if b.isEmpty => Vector(Vector(v))
      case V(v, b) => b.flatMap(_.flatten).map(v +: _)
    }
  }

  //for debugging
  def prettyPrint(prefix: String = "", isLast: Boolean = true, isRoot: Boolean = true): String = {
    val marker = if (isRoot) "" else if (isLast) "+-- " else "|-- "
    val currentLine = s"$prefix$marker$valueString\n"
    
    val nextPrefix = if (isRoot) "" else prefix + (if (isLast) "    " else "|   ")
    val branches = this match {
      case E() => Vector.empty
      case V(_, b) => b
    }
    
    val children = branches.zipWithIndex.map { case (child, i) =>
      child.prettyPrint(nextPrefix, i == branches.size - 1, false)
    }.mkString

    currentLine + children
  }

  private def valueString: String = this match {
    case E() => "E"
    case V(v, _) => v.toString.replace("\n","\\n").replace("\r","\\r")
  }

  //for debugging
  override def toString:String={
    this.match{
      case E() => "E"
      case V(v,b)=>{
        s"V($v, ${b.map(_.toString).mkString("[",",","]")})".replace("\n","\\n").replace("\r","\\r")
      }
    }
  }
}

object Tree {
  def merge[T](trees: Vector[Tree[T]]): Vector[Tree[T]] = {
    val vs = trees.collect { case v: Tree.V[T] => v }
    val merged = vs.groupBy(_.value).map { case (value, group) =>
      Tree.V(value, merge(group.flatMap(_.branch)))
    }.toVector
    
    val hasEmpty = trees.exists { case Tree.E() => true; case _ => false }
    if (hasEmpty) merged :+ Tree.E() else merged
  }
}