package romanesco.Utils
import scala.collection.immutable.Map
import scala.util.matching.Regex
import Debug.logger
import romanesco.Parser._
import romanesco.Utils.Debug
import romanesco.Registory
object Misc {
  object Prelude {
    def setup: Registory = {
      logger.log("Prelude.setup begin")
      val toknizerule: Map[String, Regex] = Map(
        // TODO:トークナイズルールを考える
        "hello" -> "hello".r,
        "world" -> "world".r,
        "space" -> "\\s+".r
      )
      logger.log(s"toknizerule:$toknizerule")
      val parserule: Map[String, ParseRule] = Map(
        // TODO:パースルールを考える
        "Greeting" -> new StandardRule(
          name = "Greeting",
          pattern = Vector(
            Predicates.matches {
              case (_, _, reg: Regex, content: String) =>
                content == "hello" && reg.toString == "hello".r.toString
              case null => false
            },
            Predicates.matches {
              case (_, _, reg: Regex, content: String) =>
                content == "world" && reg.toString == "world".r.toString
              case null => false
            }
          ),
          build = { children =>
            // children(0) is hello, children(1) is world
            ("GreetingMatched", Vector.empty) // New node
          }
        )
      )
      logger.log(s"parserule:$parserule")
      val registory = new Registory
      registory.pushTokenizer(toknizerule)
      registory.pushParser(parserule)
      logger.log("Prelude.setup end")
      registory
    }
  }
}
