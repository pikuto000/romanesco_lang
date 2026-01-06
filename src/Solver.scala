package romanesco
import com.microsoft.z3._
import scala.collection.mutable.Map
import scala.util.parsing.combinator._
import scala.util.parsing.input.OffsetPosition
import scala.util.parsing.input.Positional

class Solver {
  //Z3のコンテキスト
  private lazy val ctx = new Context()
  //Z3の最適化器
  private lazy val optimize = ctx.mkOptimize()
  //Z3のソルバー
  private lazy val solver = ctx.mkSolver()
  //Z3のファクトリ (ASTFactoryはContextから直接取得できないため、必要に応じてASTBuilderなどを使用)
  // private lazy val factory = ctx.getASTFactory()
  
  //Z3のASTを構築するためのヘルパー
  object ast {
    def mkBoolSort(): BoolSort = ctx.mkBoolSort()
    def mkIntSort(): IntSort = ctx.mkIntSort()
    def mkRealSort(): RealSort = ctx.mkRealSort()

    def mkBoolConst(name: String): BoolExpr = ctx.mkBoolConst(name)
    def mkIntConst(name: String): IntExpr = ctx.mkIntConst(name)
    def mkRealConst(name: String): RealExpr = ctx.mkRealConst(name)

    def mkTrue(): BoolExpr = ctx.mkTrue()
    def mkFalse(): BoolExpr = ctx.mkFalse()

    def mkAnd(args: BoolExpr*): BoolExpr = ctx.mkAnd(args*)
    def mkOr(args: BoolExpr*): BoolExpr = ctx.mkOr(args*)
    def mkNot(arg: BoolExpr): BoolExpr = ctx.mkNot(arg)
    def mkImplies(arg1: BoolExpr, arg2: BoolExpr): BoolExpr = ctx.mkImplies(arg1, arg2)
    def mkIff(arg1: BoolExpr, arg2: BoolExpr): BoolExpr = ctx.mkIff(arg1, arg2)

    def mkEq(arg1: Expr[?], arg2: Expr[?]): BoolExpr = ctx.mkEq(arg1, arg2)
    def mkDistinct(args: Expr[?]*): BoolExpr = ctx.mkDistinct(args*)
    def mkGt(arg1: ArithExpr[?], arg2: ArithExpr[?]): BoolExpr = ctx.mkGt(arg1, arg2)
    def mkGe(arg1: ArithExpr[?], arg2: ArithExpr[?]): BoolExpr = ctx.mkGe(arg1, arg2)
    def mkLt(arg1: ArithExpr[?], arg2: ArithExpr[?]): BoolExpr = ctx.mkLt(arg1, arg2)
    def mkLe(arg1: ArithExpr[?], arg2: ArithExpr[?]): BoolExpr = ctx.mkLe(arg1, arg2)
  }
}
