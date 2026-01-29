package romanesco
import scala.util.matching.Regex
import scala.util.boundary
import Debug.logger

object Main {
  def main(args: Array[String]): Unit = {
    println("this is the romanesco toolchain.")
    val fileContents=GetFile.importfile(args)._1
    val options=GetFile.importfile(args)._2
    if(options.contains("-debug"))Debug.logger.switch(true)
    if (fileContents.isEmpty) {
      println("Usage: \"<files and options...>\" ")
    } else {
      val registory=new Registory()
      registory.pushTokenizer(
        Map(
          "hello" -> "hello".r,
          "world" -> "world".r,
          "space" -> "\\s+".r
        )
      )
      registory.pushParser(
        Map(
          "Greeting" -> new StandardRule(
            name = "Greeting",
            pattern = Vector(
              Predicates.matches { case (_, _, _, content: String) => content == "hello" case _ => false },
              Predicates.matches { case (_, _, _, content: String) => content == "world" case _ => false }
            ),
            build = { children =>
              // children(0) is hello, children(1) is world
              ("GreetingMatched", Vector(children(0)._4, children(1)._4)) // New node
            }
          )
        )
      )
      val results=fileContents.map(f=>
        registory.run(f)
      )
    }
    println("done.")
  }
}