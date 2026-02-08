package romanesco.Utils
import scala.util.matching.Regex
import Debug.logger
import scala.util.boundary
import romanesco.Types._
import romanesco.Utils.Debug

//init is a data to be rewrited. expander is a function that rewrites init. expandrule is a rule that expands init and erase itself from init.
final class Macro[T, R](init: T, expander: (T, R) => T, expandrule: R) {
  private var material: T = init
  def dump: T = material
  def run(fuel: UInt = 1000, m: T = this.material): Unit = {
    boundary {
      logger.log("expander started")
      for (i: UInt <- 0 to fuel) {
        logger.log(s"trying to expand. now ${
            val str: String = i.toString;
            str match {
              case s if s.endsWith("1") && !s.endsWith("") => s + "st"
              case s if s.endsWith("2") && !s.endsWith("") => s + "nd"
              case s if s.endsWith("3") && !s.endsWith("") => s + "rd"
              case s                                       => s + "th"
            }
          } try.")
        lazy val old = material
        lazy val expanded = expander(material, expandrule)
        logger.log("expand succseeded.")
        if (old != expanded) {
          logger.log("expand makes changes.")
          material = expanded
        } else {
          logger.log("expand makes no changes. exitting...")
          boundary.break()
        }
      }
    }
    logger.log("expander finished\n")
  }
}

/*超簡易的なプリプロセッサを実装してテスト
@main def macrotest:Unit ={
  logger.switch(true)
  val testString="define X 100 X"
  val expandRule= "define X 100"
  println("this is a macro tester.\n")
  println(s"target string is \"$testString\".")
  println(s"expand rule is \"$expandRule\".\n")
  val expander:(String,String)=>String=
    (str,rule)=>{
      //strの中にある文字列のマッチする部分を消去し、Xを100に置換する
      val parts = rule.split(" ")
      if (parts.length >= 3 && parts(0) == "define") {
        val target = parts(1)
        val replacement = parts(2)
        str.replace(rule, "").replace(target, replacement).trim
      } else {
        str
      }
    }
  val macrotester=new Macro(testString,expander,expandRule)
  macrotester.run()
  println(s"result:${macrotester.dump}")
}
 */
