package romanesco
object Main {
  import scala.io.Source
  def main(args: Array[String]): Unit = {
    println("this is the romanesco toolchain.")
    if (true==args.isEmpty) {
      println("Usage: sbt \"run <files and options...>\" ")
    }else{
      val len=args.length
      //tempolary
      println(s"i caught $len arguments. ending run...")
    }
    println("done.")
  }
}
