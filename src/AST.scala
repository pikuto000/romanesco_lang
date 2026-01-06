package romanesco
import scala.util.parsing.input.Positional

// Romanescoの基本抽象構文木
trait AST extends Positional

object AST {
  // 識別子（変数）: HygenicTagを保持し、Z3の変数と紐付く
  case class Variable(tag: HygenicTag) extends AST {
    override def toString: String = s"Var(${tag.mangledName})"
  }

  // リテラル（定数）
  case class IntLiteral(value: BigInt) extends AST
  case class BoolLiteral(value: Boolean) extends AST

  // 単一化 / 制約追加: "X = 5" や "X = Y"
  case class Unification(left: AST, right: AST) extends AST {
    override def toString: String = s"($left = $right)"
  }

  // 命令の並び
  case class Block(statements: Array[AST]) extends AST
}
