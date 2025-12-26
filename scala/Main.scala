import parser._
import parser.init._
import parser.interpreter._
import java.io.File
import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.Parsers.Failure

object Main{
    //コマンドライン引数か、標準入力からファイル名を取得、ファイルのテキストをパースする
    def main(args: Array[String]): Unit = {
        val sym = setup
        val parser = new rParser(sym)
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

        var offset = 0
        var continue = true
        
        while(continue && offset < source.length) {
            if (source.subSequence(offset, source.length).toString.trim.isEmpty) {
                continue = false
            } else {
                val input = new CharSequenceReader(source, offset)
                val packratInput = new parser.PackratReader(input)
                val currentParser = new rParser(sym)
                currentParser.parse(currentParser.expr, packratInput) match {
                    case currentParser.Success(node, next) =>
                        eval(node, sym)
                        offset = next.offset
                    case currentParser.NoSuccess(msg, next) =>
                        println(s"Parse error at line ${next.pos.line}, column ${next.pos.column}: $msg")
                        println(next.pos.longString)
                        continue = false
                    case Failure(msg, next) => 
                        println(s"Parse error at line ${next.pos.line}, column ${next.pos.column}: $msg")
                        println(next.pos.longString)
                        continue = false
                }
            }
        }
    }
}