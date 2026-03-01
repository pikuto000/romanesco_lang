package PROTO.romanesco.Solver.core
import scala.quoted._
import scala.annotation.tailrec

/** Romanesco Lang 用・純粋カリー化 Expr by Grok (null完全排除・精神衛生最強) */
final class Expr[T] private (
    val tag: Option[
      T
    ], // Some(...) = leaf OR flat-function / None = Curried-App
    val fun: Option[Expr[T]], // Curried-Function OR Flat-Argument
    val arg: Option[Expr[T]] // Curried-Argument OR None (for Flat)
) {

  /** 適用（純粋カリー化・左結合） */
  def apply(a: Expr[T]): Expr[T] =
    new Expr(None, Some(this), Some(a))

  @tailrec
  private def collectFlat(acc: List[Expr[T]]): (Expr[T], List[Expr[T]]) = {
    tag match {
      case Some(f: Expr[T] @unchecked) if fun.isDefined && arg.isEmpty =>
        f.collectFlat(fun.get :: acc)
      case _ => (this, acc)
    }
  }

  override def toString: String = tag match {
    case Some(f: Expr[T] @unchecked) if fun.isDefined && arg.isEmpty =>
      val (h, as) = collectFlat(Nil)
      (h :: as).mkString(" ")
    case Some(t) =>
      t match {
        case s: String => s
        case other     => other.toString
      }
    case None =>
      val f = fun.get.toString
      val a = arg.get.toString
      if (f.contains(" ")) s"($f $a)" else s"$f $a"
  }

  /** ツリー表示 */
  def prettyTree(indent: String = "", isLast: Boolean = true): String = {
    val marker = "+- "
    tag match {
      // Flat Application Step (tag contains Expr, fun is argument, arg is None)
      case Some(f: Expr[T] @unchecked) if fun.isDefined && arg.isEmpty =>
        val (h, as) = collectFlat(Nil)
        val current = s"$indent$marker@App\n"
        val nextIndent = indent + (if (isLast) "    " else "|   ")
        current + s"$nextIndent$marker${(h :: as).mkString(" ")}\n"

      // Leaf
      case Some(t) => s"$indent$marker$t\n"

      // Curried Application
      case None =>
        val current = s"$indent$marker@App\n"
        val nextIndent = indent + (if (isLast) "    " else "|   ")
        current + fun.get.prettyTree(nextIndent, false) + arg.get.prettyTree(
          nextIndent,
          true
        )
    }
  }

  def showTree = prettyTree()
  def printTree = println(showTree)
}

object Expr {
  extension [T](e: Expr[T]) {

    @tailrec
    def applyN(args: Expr[T]*): Expr[T] =
      if args.isEmpty then e
      else e(args.head).applyN(args.tail*)

    // フラットな適用を作成 (tagにaccを込める)
    def applyAll(args: Expr[T]*): Expr[T] =
      args.foldLeft(e) { (acc, arg) =>
        new Expr(Some(acc.asInstanceOf[T]), Some(arg), None)
      }
  }

  /** leaf作成（括弧なし！） */
  def leaf[T](tag: T): Expr[T] =
    new Expr(Some(tag), None, None)

  private var metaCounter: Int = 0

  /** 新しいMeta発行（括弧なし！） */
  def freshMeta[T]: Expr[T] = {
    metaCounter += 1
    leaf(s"?M$metaCounter".asInstanceOf[T])
  }

  /** structural hash（unification用・カリー化対応） */
  def hash(e: Expr[Any]): Long = e.tag match {
    case Some(f: Expr[_] @unchecked) if e.fun.isDefined && e.arg.isEmpty =>
      31L * hash(f.asInstanceOf[Expr[Any]]) + hash(
        e.fun.get.asInstanceOf[Expr[Any]]
      )
    case Some(t) => t.hashCode.toLong
    case None    => 31L * hash(e.fun.get) + hash(e.arg.get)
  }

}

@main def testExpr: Unit = {
  val (one, plus) = (Expr.leaf("1"), Expr.leaf("+"))

  println("--- Curried Application (applyN) ---")
  plus.applyN(one, one).printTree

  println("\n--- Flat Application (applyAll) ---")
  plus.applyAll(one, one).printTree
}
