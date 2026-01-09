package romanesco
import scala.util.parsing.input.Positional

// Attributesを型安全に
enum AttributeValue {
  case Str(value: String)
  case Num(value: BigDecimal)
  case Bool(value: Boolean)
  case Op(value: String)
}

case class Node(
  kind: String,
  tag: HygenicTag,
  attributes: Map[String, AttributeValue] = Map.empty,
  children: List[Node] = Nil
) extends Positional {
  
  // 型安全なアクセサ
  def getStr(key: String): Option[String] = {
    attributes.get(key).collect { case AttributeValue.Str(v) => v }
  }
  
  def getNum(key: String): Option[BigDecimal] = {
    attributes.get(key).collect { case AttributeValue.Num(v) => v }
  }
  
  def getOp(key: String): Option[String] = {
    attributes.get(key).collect { case AttributeValue.Op(v) => v }
  }
  
  def getBool(key: String): Option[Boolean] = {
    attributes.get(key).collect { case AttributeValue.Bool(v) => v }
  }
  
  // 便利メソッド：必須の属性を取得
  def requireOp(key: String): String = {
    getOp(key).getOrElse {
      throw new IllegalStateException(s"Missing required operator attribute: $key in $kind")
    }
  }
  
  def requireNum(key: String): BigDecimal = {
    getNum(key).getOrElse {
      throw new IllegalStateException(s"Missing required numeric attribute: $key in $kind")
    }
  }
  
  def requireStr(key: String): String = {
    getStr(key).getOrElse {
      throw new IllegalStateException(s"Missing required string attribute: $key in $kind")
    }
  }
  
  override def toString: String = {
    val attrs = if (attributes.nonEmpty) { attributes.toString } else { "" }
    val kids = if (children.nonEmpty) { children.mkString("(", ", ", ")") } else { "" }
    s"$kind$attrs$kids"
  }
}
