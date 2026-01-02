import parser._
import parser.init._
import parser.interpreter._
import java.io.File

object Main{
    //コマンドライン引数か、標準入力からファイル名を取得、ファイルのテキストをパースする
    def main(args: Array[String]): Unit = {
        lazy val sym = setup
        lazy val source = if (args.length > 0) {
            lazy val file = new File(args(0))
            if (!file.exists()) {
                println(s"Error: File ${args(0)} not found.")
                sys.exit(1)
            }
            scala.io.Source.fromFile(file).mkString
        } else {
            print("input file name > ")
            lazy val filename = scala.io.StdIn.readLine()
            lazy val file = new File(filename)
            if (!file.exists()) {
                println(s"Error: File $filename not found.")
                sys.exit(1)
            }
            scala.io.Source.fromFile(file).mkString
        }

        // --- Execute via Context ---
        lazy val ctx = new RomanescoContext("main", sym)
        ctx.run(source)
        sym.close()
    }
}