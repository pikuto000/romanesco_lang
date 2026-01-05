package romanesco

object Main {
  def main(args: Array[String]): Unit = {
    //デバッグ用
    logger.Switch(true)
    //ファイルをコマンドライン引数から読み込む
    val filename = if (args.length==1){args(0)}else{
        scala.io.StdIn.readLine("input file:")
    }
    val source = scala.io.Source.fromFile(filename)
    val reader = source.reader()
    val lexer = InstanceRegistory.launch()
    lexer(reader)
    lexer.printStream
  }
}