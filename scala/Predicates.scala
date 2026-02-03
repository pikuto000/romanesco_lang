package romanesco
import scala.util.matching.Regex

object Predicates {
  // Assuming Token type matches Tokenizer's definition
  // Tuple4[Row, Col, TokenType, Content]
  type Token = Tokenizer#Token
  type TokenTree = Tree[Token]

  trait Predicate {
    def check(t: Token): Boolean
  }

  //basic match predicate
  def matches(f: Token => Boolean): Predicate = new Predicate {
    def check(t: Token): Boolean = f(t)
  }

  //either predicate
  def either(p1: Predicate, p2: Predicate): Predicate = new Predicate {
    def check(t: Token): Boolean = p1.check(t) || p2.check(t)
  }

  //repeat predicate n times except stop condition is met
  def repeat(p: Predicate, n: UInt, stopl: Predicate = null): Predicate = new Predicate {
    def check(t: Token): Boolean = {
      var count: UInt= 0
      var current = t
      while (count < n && (stopl == null || !stopl.check(current))) {
        if (p.check(current)) {
          count += 1
        } else {
          return count >= n
        }
      }
      count >= n
    }
  }
}