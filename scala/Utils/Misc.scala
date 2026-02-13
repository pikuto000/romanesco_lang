// ==========================================
// Misc.scala
// ユーティリティ関数（循環検知・項の比較）
// ==========================================

package romanesco.Utils

import romanesco.Solver.core.Expr

object Misc {
  
  /** 項の「埋め込み」判定（Homeomorphic Embedding）
    * 
    * 式 e1 が e2 に埋め込まれている (e1 ⊴ e2) とは、
    * e2 の一部を削除して e1 に変形できることを指します。
    */
  def isEmbedding(e1: Expr, e2: Expr): Boolean = {
    // 1. カップリング規則
    def coupling: Boolean = (e1, e2) match {
      case (Expr.Sym(s1), Expr.Sym(s2)) => s1 == s2
      case (Expr.Var(v1), Expr.Var(v2)) => v1 == v2
      case (Expr.Meta(m1), Expr.Meta(m2)) => m1 == m2
      case (Expr.App(f1, a1), Expr.App(f2, a2)) if a1.length == a2.length =>
        isEmbedding(f1, f2) && a1.zip(a2).forall((x, y) => isEmbedding(x, y))
      case _ => false
    }

    // 2. ダイビング規則
    def diving: Boolean = e2 match {
      case Expr.App(f, args) => isEmbedding(e1, f) || args.exists(a => isEmbedding(e1, a))
      case _ => false
    }

    coupling || diving
  }
}
