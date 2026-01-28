package romanesco

object Main {
  def main(args: Array[String]): Unit = {
    println("this is the romanesco toolchain.")
    if (args.isEmpty) {
      println("Usage: sbt \"run <files and options...>\" ")
    } else {
      println("this is a test. just print file contents.")
      val len = args.length
      // temporaly, test file system.
      val files = GetFile.importfile(args)
      println("file contents:")
      files._1.foreach(println)
      println("\noptions:")
      files._2.foreach(println)
    }
    println("done.")
  }
}