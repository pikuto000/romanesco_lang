package romanesco
import com.microsoft.z3._

object Hygenicmarker {
    import scala.util.hashing.MurmurHash3
    
    // ユニークなIDを生成するためのカウンター
    private var instanceCounter = 0
    private def nextId(): Int = {
        instanceCounter += 1
        instanceCounter
    }

    def bless(name:String, ancestor:Option[HygenicObj], isOpaque:Boolean=false): HygenicTag = {
        val tag = new HygenicTag(
            name,
            MurmurHash3.stringHash(name + (ancestor match {
                case Some(obj) => obj.tag.mangledName
                case None => ""
            })),
            ancestor match {
                case Some(obj) => obj.tag.hash
                case None => 0
            },
            isOpaque
        )
        logger.log(s"[hygenic] Blessed ${name} (Hash: ${tag.hash})")
        tag
    }

    // マクロ展開時などに、元のタグをベースに「この世に一つだけのコピー」を作る
    def freshen(base: HygenicTag): HygenicTag = {
        val id = nextId()
        val newTag = new HygenicTag(
            base.name,
            MurmurHash3.stringHash(base.mangledName + id),
            base.hash,
            base.isOpaque
        )
        // logger.log(s"[hygenic] Freshened ${base.name} -> ${newTag.mangledName}")
        newTag
    }
}

class HygenicTag(
    val name: String,
    val hash: Int,
    val ancestorHash: Int,
    val isOpaque: Boolean,
    var constraint: Option[Seq[Z3Object]] = None
){
    def mangledName: String = s"${name}_#$hash"
}

trait HygenicObj(t: HygenicTag){
    val tag = t
    def isChildOf(other: HygenicObj): Boolean = tag.ancestorHash == other.tag.hash
}
