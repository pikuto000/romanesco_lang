package PROTO.romanesco.Solver.core
import scala.annotation.tailrec

/** Romanesco Lang 用・純粋カリー化 Expr */
final class Expr[T] private (
    val tag: Option[T],       // Some(t) = 葉
    val fun: Option[Expr[T]], // None = 葉 / Some(f) = 適用の関数部
    val arg: Option[Expr[T]]  // None = 葉 / Some(a) = 適用の引数部
) {
  /** 1引数カリー化適用 */
  def apply(a: Expr[T]): Expr[T] =
    new Expr(None, Some(this), Some(a))

  override def toString: String = tag match {
    case Some(t) => t match {
      case s: String => s
      case other     => other.toString
    }
    case None =>
      val f = fun.get.toString
      val a = arg.get.toString
      if f.contains(" ") then s"($f) $a" else s"$f $a"
  }

  /** ツリー表示 */
  def prettyTree(indent: String = "", isLast: Boolean = true): String = {
    val marker = "+- "
    tag match {
      case Some(t) => s"$indent$marker$t\n"
      case None =>
        val nextIndent = indent + (if isLast then "    " else "|   ")
        s"$indent${marker}@App\n" +
          fun.get.prettyTree(nextIndent, false) +
          arg.get.prettyTree(nextIndent, true)
    }
  }

  def showTree  = prettyTree()
  def printTree = println(showTree)
}

object Expr {

  // ─── 基本コンストラクタ ────────────────────────────────────────────
  def leaf[T](tag: T): Expr[T] = new Expr(Some(tag), None, None)

  private var metaCounter: Int = 0

  /** 新しいメタ変数を発行 */
  def freshMeta: Expr[String] = { metaCounter += 1; leaf(s"?M$metaCounter") }

  // ─── 適用（コンパニオン静的メソッド） ─────────────────────────────
  /** カリー化適用（1引数）: applyC(f, a) = f(a) */
  def applyC[T](f: Expr[T], a: Expr[T]): Expr[T] = f(a)

  /** フラット適用（多引数）: applyF(f, a1, a2) = f(a1)(a2) */
  def applyF[T](f: Expr[T], args: Expr[T]*): Expr[T] =
    args.foldLeft(f)(_ apply _)

  // ─── 変換（カリー化ツリー ↔ スパイン） ────────────────────────────
  /** カリー化ツリー → (head, 引数リスト)
   *  例: App(App(f, a1), a2) → (f, List(a1, a2)) */
  def spine[T](e: Expr[T]): (Expr[T], List[Expr[T]]) =
    if e.tag.isEmpty then
      val (h, args) = spine(e.fun.get)
      (h, args :+ e.arg.get)
    else (e, Nil)

  /** (head, 引数リスト) → カリー化ツリー
   *  例: (f, List(a1, a2)) → App(App(f, a1), a2) */
  def fromSpine[T](head: Expr[T], args: List[Expr[T]]): Expr[T] =
    args.foldLeft(head)(_ apply _)

  // ─── extension（インスタンスメソッド風の多引数適用） ─────────────
  extension [T](e: Expr[T]) {
    /** カリー化多引数適用: e.applyN(a1, a2) = e(a1)(a2) */
    @tailrec
    def applyN(args: Expr[T]*): Expr[T] =
      if args.isEmpty then e
      else e(args.head).applyN(args.tail*)
  }

  // ─── Term = Expr[String] 用の命名規約 ───────────────────────────────
  // シンボル : "plus", "∀", "cons"  etc. (プレフィックスなし)
  // 変数     : "v:x", "v:n"          etc. ("v:" プレフィックス)
  // メタ変数 : "?M1", "?M2"          etc. ("?M" プレフィックス)

  type Term = Expr[String]

  def sym(name: String): Term = leaf(name)
  def v(name: String): Term   = leaf(s"v:$name")

  def isSym(tag: String): Boolean  = !tag.startsWith("v:") && !tag.startsWith("?M")
  def isVar(tag: String): Boolean  = tag.startsWith("v:")
  def isMeta(tag: String): Boolean = tag.startsWith("?M")

  // ─── パターンマッチ用エクストラクタ ────────────────────────────────
  object Sym  { def unapply(e: Term): Option[String] = e.tag.filter(isSym) }
  object Var  { def unapply(e: Term): Option[String] = e.tag.filter(isVar).map(_.stripPrefix("v:")) }
  object Meta { def unapply(e: Term): Option[String] = e.tag.filter(isMeta) }
  object App  {
    def unapply(e: Term): Option[(Term, Term)] =
      if e.tag.isEmpty then Some((e.fun.get, e.arg.get)) else None
  }
  object Apps {
    def unapply(e: Term): Option[(Term, List[Term])] =
      spine(e) match {
        case (h, args) if args.nonEmpty => Some((h, args))
        case _                          => None
      }
  }

  // ─── Term 専用表示（v: プレフィックスを除去） ────────────────────────
  extension (e: Term) {
    def show: String = e match {
      case Var(n)    => n
      case Sym(s)    => s
      case Meta(m)   => m
      case App(f, a) =>
        val fs = f.show
        val as = a.show
        if fs.contains(" ") then s"($fs) $as" else s"$fs $as"
    }
    def printShow = println(e.show)
  }

  /** structural hash（unification用） */
  def hash(e: Expr[Any]): Long = e.tag match {
    case Some(t) => t.hashCode.toLong
    case None    => 31L * hash(e.fun.get) + hash(e.arg.get)
  }
}

@main def testExpr: Unit = {
  import Expr._

  val plus = sym("plus")
  val n    = v("n")
  val m    = v("m")
  val term = applyF(plus, n, m)   // フラット適用

  println("--- show ---")
  term.printShow                  // plus n m

  println("\n--- ツリー表示 ---")
  term.printTree

  println("\n--- スパイン変換 ---")
  val (head, args) = spine(term)
  println(s"head = ${head.show}, args = ${args.map(_.show)}")  // head = plus, args = List(n, m)
  println(fromSpine(head, args).show)                           // plus n m（元に戻る）

  println("\n--- Apps エクストラクタ ---")
  term match {
    case Apps(Sym(f), List(a, b)) => println(s"$f を $a と $b に適用")
    case _                        => println("マッチなし")
  }

  println("\n--- applyC（カリー化1ステップ）---")
  val step = applyC(plus, n)      // plus n
  step.printShow

  println("\n--- freshMeta ---")
  val mx = freshMeta
  println(mx.show)                // ?M1
}
