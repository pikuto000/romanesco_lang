package romanesco
import com.microsoft.z3._
object Hygenicmarker {
    import scala.util.hashing.MurmurHash3
    import scala.collection.mutable.Map
    private def mark(path:String):Int=
        MurmurHash3.stringHash(path)

    def bless(name:String,ancestor:Option[HygenicObj],isOpaque:Boolean=false):HygenicTag={
        lazy val tag=new HygenicTag(
            name,
            MurmurHash3.stringHash(name + (ancestor match{
                case Some(obj:HygenicObj) => obj.tag.name
                case None => ""
            })),
            ancestor match {
                case Some(obj) => MurmurHash3.stringHash(obj.tag.name)
                case None => 0 // Or some other appropriate default/error handling
            },
            isOpaque
        )
        logger.log(s"[hygenic] Hygenicmarker marked ${name}, Hash is ${tag.hash}, ancestorHash is ${tag.ancestorHash}, isOpaque is ${tag.isOpaque}.")
        tag
    }
}

class HygenicTag(
    val name:String,
    val hash:Int,
    val ancestorHash:Int,
    val isOpaque:Boolean,
    var constriant:Option[Seq[Z3Object]]=None
){
    def mangledName:String=s"${name}_#$hash"
    def DumpConstriant:String={
        constriant match{
            case Some(c)=>c.map(x=>x.toString).mkString("\n")
            case None=>""
        }
    }
}

trait HygenicObj(t:HygenicTag){
    lazy val tag=t
    def checkfamily(yaho:HygenicObj)=if (tag.ancestorHash==yaho.tag.hash)true else false
}