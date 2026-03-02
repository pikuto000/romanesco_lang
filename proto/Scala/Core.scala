enum Expr[T]:
  case Leaf(value: T)
  private case App(fun: Expr[T], arg: Expr[T])
  private case FlatApp(fun: Expr[T], args: Vector[Expr[T]])

object Expr:
  def id[T]: Expr[T => T] = Leaf((x: T) => x)

  extension [T](e: Expr[T])
    def apply(args: Expr[T]*): Expr[T] =
      if args.size == 1 then App(e, args.head) else FlatApp(e, args.toVector)

  private implicit class FlatAppOps[T](fa: Expr[T]):
    def apply(args: Expr[T]*): Expr[T] =
      if args.size == 1 then App(fa, args.head) else FlatApp(fa, args.toVector)

  // ==================== 相互変換 ====================
  extension [T](e: Expr[T])
    def toFlat: Expr[T] = e match
      case FlatApp(_, _) => e
      case _             =>
        def collect(
            cur: Expr[T],
            acc: Vector[Expr[T]] = Vector.empty
        ): (Expr[T], Vector[Expr[T]]) = cur match
          case App(f, a) =>
            val (h, t) = collect(f, acc)
            (h, t :+ a)
          case _ => (cur, acc)
        val (head, args) = collect(e)
        if (args.isEmpty) head else FlatApp(head, args)

    def toCurried: Expr[T] = e match
      case FlatApp(fun, args) =>
        args.foldLeft(fun)((acc, arg) => App(acc, arg))
      case _ => e

  // ==================== prettyTree ====================
  extension [T](e: Expr[T])
    def simpleShow: String = e match
      case Leaf(v)          => v.toString
      case App(f, a)        => s"${f.simpleShow}(${a.simpleShow})"
      case FlatApp(f, args) =>
        s"${f.simpleShow}(${args.map(_.simpleShow).mkString(", ")})"

    def prettyTree(indent: String = "", isLast: Boolean = true): String =
      def collect(
          cur: Expr[T],
          acc: Vector[Vector[Expr[T]]]
      ): (Expr[T], Vector[Vector[Expr[T]]]) = cur match
        case App(f, a)        => collect(f, Vector(a) +: acc)
        case FlatApp(f, args) => collect(f, args +: acc)
        case _                => (cur, acc)

      val (head, apps) = collect(e, Vector.empty)
      val prefix = if isLast then "+-- " else "+-- "

      val headStr = head match
        case Leaf(v) => indent + prefix + v.toString + "\n"
        case _       => head.prettyTree(indent, apps.isEmpty && isLast)

      val appsStr = apps.map { args =>
        indent + "|  +-- Apply(" + args.map(_.simpleShow).mkString(", ") + ")\n"
      }.mkString

      headStr + appsStr

  def showTree[T](e: Expr[T]): String = e.prettyTree()

@main def testExpr: Unit = {
  val leaf = Expr.Leaf
  val id = Expr.id
  val f = leaf("f")
  val g = leaf("g")
  val h = leaf("h")
  val a = leaf("a")
  val b = leaf("b")
  val c = leaf("c")
  val d = leaf("d")
  val e = leaf("e")

  // f(g(a, b), c)(d, h(e))
  val terms = f(g(a, b), c)(d, h(e))

  println(terms.prettyTree())
}
