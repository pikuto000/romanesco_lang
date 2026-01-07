package romanesco
import scala.util.hashing.MurmurHash3

case class HygenicTag(
  name: String, 
  hash: Int, 
  isOpaque: Boolean = false // 追加: このタグを持つ実体が論理的に隔離されているか
) {
  def mangledName: String = s"${name}_#${hash}"
}

trait HygenicObj(val tag: HygenicTag)

object Hygenicmarker {
  private var globalCounter = 0L

  def bless(name: String, parent: Option[Any] = None, unique: Boolean = true): HygenicTag = {
    val baseHash = parent match {
      case Some(p) => MurmurHash3.productHash((name, p.hashCode()))
      case None    => MurmurHash3.stringHash(name)
    }
    // unique が真なら、実行ごとに一意なハッシュを生成
    val finalHash = if (unique) {
      globalCounter += 1
      MurmurHash3.productHash((baseHash, globalCounter.toInt, java.util.UUID.randomUUID().hashCode()))
    } else baseHash
    HygenicTag(name, finalHash)
  }

  // 特定の seed（マクロ呼び出しのハッシュなど）を使ってタグを新鮮にする
  def freshen(base: HygenicTag, seed: Int): HygenicTag = {
    val newHash = MurmurHash3.productHash((base.hash, seed))
    base.copy(hash = newHash)
  }

  // 論理的に不透明（Solver が無視すべき）なタグに変換
  def makeOpaque(base: HygenicTag): HygenicTag = base.copy(isOpaque = true)
}
