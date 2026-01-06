package romanesco
//時間を測る
import java.time.Instant
import java.time.Duration

object Main {
  def main(args: Array[String]): Unit = {
    //ファイルをコマンドライン引数から読み込む
    lazy val filename = if (args.length>=1){args(0)}else{
        scala.io.StdIn.readLine("input file:")
    }
    lazy val debugflag = if (args.length==2){args(1)=="debug"}else{false}
    if (debugflag){logger.Switch(true)}
    lazy val source = scala.io.Source.fromFile(filename)
    lazy val reader = source.reader()
    lazy val lexer = InstanceRegistory.launch()
    lazy val parser = InstanceRegistory.launchParser(lexer)
    //計測開始
    val start = Instant.now()
    val lattice = lexer(reader)
    if (true==debugflag)lexer.printStream
    
    // パース実行
    val parseResult = parser(lattice.asInstanceOf[Map[Int, Array[parser.lexer.Token]]])
    
    if (debugflag) parser.printStream

    lazy val end = Instant.now()
    lazy val duration = Duration.between(start, end)
    println(s"opertion took ${duration.toMillis()} ms")
  }
}