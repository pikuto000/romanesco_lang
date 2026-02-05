package romanesco
import scala.util.matching.Regex
import scala.util.boundary
import Debug.logger
/*
object Main {
  def main(args: Array[String]): Unit = {
    println("this is the romanesco toolchain.")
    val fileContents=GetFile.importfile(args)._1
    val options=GetFile.importfile(args)._2
    if(options.contains("-debug"))Debug.logger.switch(true)
    if (fileContents.isEmpty) {
      println("Usage: \"<files and options...>\" ")
    } else {
      val registory=Misc.Prelude.setup
      val results=fileContents.map(f=>
        registory.run(f)
      )
    }
    println("done.")
  }
}
*/