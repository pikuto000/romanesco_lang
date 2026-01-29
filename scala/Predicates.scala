package romanesco
import scala.util.matching.Regex

object Predicates {
  // Assuming Token type matches Tokenizer's definition
  // Tuple4[Row, Col, TokenType, Content]
  type Token = Tuple4[Int, Int, Regex, String]

  trait Predicate {
    def check(t: Token): Boolean
  }

  def matches(f: Token => Boolean): Predicate = new Predicate {
    def check(t: Token): Boolean = f(t)
  }
}