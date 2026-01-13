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
    val visited = scala.collection.mutable.Set.empty[Int]

    def loop(t: tree[A], prefix: String, isLast: Boolean, isRoot: Boolean): String =
      var curr = t
      val chain = new StringBuilder()
      var branches: LazyList[tree[A]] = LazyList.empty
      var stop = false

      while !stop do
        val id = System.identityHashCode(curr)
        if visited.contains(id) then
           if chain.nonEmpty then chain.append(" -> ")
           chain.append(s"(ref: @${Integer.toHexString(id)})")
           stop = true
        else
           visited += id
           if chain.nonEmpty then chain.append(" -> ")

           curr match
             case DeadEnd =>
               chain.append("DeadEnd")
               stop = true
             case Node(vals, brs) =>
               chain.append(s"${vals.mkString(", ")} @${Integer.toHexString(id)}")
               brs match
                 case LazyList() =>
                   stop = true
                 case h #:: LazyList() =>
                   curr = h
                 case _ =>
                   branches = brs
                   stop = true

      val (marker, childIndent) =
        if isRoot then ("", "")
        else if isLast then ("└── ", "    ")
        else ("├── ", "│   ")

      val header = s"$prefix$marker$chain"

      if branches.isEmpty then
         header + "\n"
      else
         val body = branches.zipWithIndex.map { (b, i) =>
           loop(b, prefix + childIndent, i == branches.length - 1, false)
         }.mkString
         header + "\n" + body

    loop(this, "", true, true)