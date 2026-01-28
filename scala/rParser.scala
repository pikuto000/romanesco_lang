package romanesco
import scala.quoted._
import scala.collection.mutable
import Debug.logger
// =====================
// rParser 本体
// =====================

final class rParser[A, B](rules: mutable.Map[String, ParseRule[A, B]]) {

  // (Tree, ruleName) -> 結果
  private val cache =
    mutable.Map[(Tree[Any], String), Vector[Tree[Any]]]()

  def parse(tree: Tree[A]): Tree[B] = {

    logger.log("[rParser] start parsing")

    // 1ステップ：全ルールをこの Tree に適用
    def step(t: Tree[Any]): Vector[Tree[Any]] = {
      rules.values.toVector.flatMap { rule =>
        val key = (t, rule.name)
        cache.getOrElseUpdate(
          key, {
            logger.log(s"[rParser] applying rule: ${rule.name}")
            rule
              .asInstanceOf[ParseRule[Any, Any]]
              .apply(t)
              .asInstanceOf[Vector[Tree[Any]]]
          }
        )
      }
    }

    // fixpoint（飽和）探索
    def saturate(
      frontier: Vector[Tree[Any]],
      seen: Set[Tree[Any]]
    ): Set[Tree[Any]] = {

      if (frontier.isEmpty) seen
      else {
        val next =
          frontier
            .flatMap(step)
            .filterNot(seen.contains)

        saturate(next, seen ++ next)
      }
    }

    val start = tree.asInstanceOf[Tree[Any]]

    val allResults =
      saturate(Vector(start), Set(start))

    logger.log(s"[rParser] total results: ${allResults.size}")

    // 全結果を 1 つの Tree に統合（非決定性保持）
    Tree.V(
      value = "PARSE_RESULT".asInstanceOf[B],
      branch = allResults.toVector.map(_.asInstanceOf[Tree[B]])
    )
  }
}

trait ParseRule[A, B] {
  val name: String
  val pattern: Vector[Predicate[A]]
  val build: Vector[Tree[B]] => Tree[B]
  def apply(tree: Tree[A]): Vector[Tree[Any]]
}

trait Predicate[A] {
  def apply(a: A): Boolean
}