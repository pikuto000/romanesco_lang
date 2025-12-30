import parser._
import parser.init._
import parser.interpreter._
import java.io.File

object Main{
    //コマンドライン引数か、標準入力からファイル名を取得、ファイルのテキストをパースする
    def main(args: Array[String]): Unit = {
        val sym = setup
        val source = if (args.length > 0) {
            val file = new File(args(0))
            if (!file.exists()) {
                println(s"Error: File ${args(0)} not found.")
                sys.exit(1)
            }
            scala.io.Source.fromFile(file).mkString
        } else {
            print("input file name > ")
            val filename = scala.io.StdIn.readLine()
            val file = new File(filename)
            if (!file.exists()) {
                println(s"Error: File $filename not found.")
                sys.exit(1)
            }
            scala.io.Source.fromFile(file).mkString
        }

        // --- Execute via Context ---
        val ctx = new RomanescoContext("main", sym)
        ctx.run(source)
    }
}