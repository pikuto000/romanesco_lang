package romanesco
//時間を測る
import java.time.Instant
import java.time.Duration

object Main {
  def main(args: Array[String]): Unit = {
    //ファイルをコマンドライン引数から読み込む
    val filename = if (args.length>=1){args(0)}else{
        scala.io.StdIn.readLine("input file:")
    }
    val debugflag = if (args.length==2){args(1)=="debug"}else{false}
    if (debugflag){logger.Switch(true)}
    val source = scala.io.Source.fromFile(filename)
    // レキサーのセットアップと機能注入
    val lexer = init.lex.setup("firstLexer")
    init.lex.literals(lexer)
    init.lex.operators(lexer)

    // パーサーのセットアップと機能注入
    val parser = init.parse.setup("firstParser", lexer)
    init.parse.literals(parser)
    init.parse.logic(parser)

    //読み込み
    val reader = source.bufferedReader()
    //計測開始
    val start = Instant.now()
    val lattice = lexer(reader)
    if (true==debugflag)lexer.printStream
    
    // パース実行
    val parseResult = parser(lattice.asInstanceOf[Map[Int, Array[parser.lexer.Token]]])
    
    if (debugflag) parser.printStream

    // 制約解決の実行
    val solver = new Solver()
    parseResult match {
      case parser.Success(res, _) =>
        init.semantics.execute(res, solver)
      case _ =>
        println("Skipping execution due to parse failure.")
    }

    val end = Instant.now()
    val duration = Duration.between(start, end)
    println(s"opertion took ${duration.toMillis()} ms")
  }
}