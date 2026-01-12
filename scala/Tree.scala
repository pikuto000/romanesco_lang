package Undeterminable

enum tree[+A]:
  case Node(values: Vector[A], branches: LazyList[tree[A]])
  case DeadEnd

  def flattenPaths: LazyList[LazyList[A]] = this match
    case DeadEnd => LazyList.empty[LazyList[A]]
    case Node(vals, LazyList()) => LazyList(LazyList.from(vals))
    case Node(vals, brs) if brs.nonEmpty => lazy val prefix =LazyList.from(vals)
      brs.flatMap(b => b.flattenPaths).map(prefix #::: _)
    case _ => LazyList.empty[LazyList[A]]

  def drawTree: String =
    lazy val visited = scala.collection.mutable.Set.empty[Int]
    def loop(t: tree[A], indent: String, isLast: Boolean): String =
      lazy val id = System.identityHashCode(t)
      lazy val marker = if isLast then "└── " else "├── "
      lazy val nextIndent = indent + (if isLast then "    " else "│   ")
      
      lazy val head = indent + marker + (t match
        case DeadEnd => "DeadEnd"
        case Node(vals, _) => s"Node@${Integer.toHexString(id)}(vals: [${vals.mkString(", ")}])"
      )

      if visited.contains(id) then head + " (already seen)\n"
      else
        visited += id
        t match
          case DeadEnd => head + "\n"
          case Node(_, brs) if brs.isEmpty => head + "\n"
          case Node(_, brs) =>
            lazy val body = brs.zipWithIndex.map { (b, i) =>
              loop(b, nextIndent, i == brs.length - 1)
            }.mkString
            head + "\n" + body

    loop(this, "", true)

  // 以前の互換性のためのメソッド（必要に応じて）
  //def map[B: scala.reflect.ClassTag](f: A => B): tree[B] = this match
    //case DeadEnd => DeadEnd
    //case Node(vals, brs) => Node(vals.map(f), brs.map(_.map(f)))
