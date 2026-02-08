package romanesco.Parser
import scala.util.matching.Regex
import scala.util.boundary
import romanesco.Types._

object Predicates {
  // Assuming Token type matches Tokenizer's definition
  // Tuple4[Row, Col, TokenType, Content]
  type Token = Tokenizer#Token
  type TokenTree = Tree[Token]

  trait Predicate {
    def check(t: Token): Boolean
  }

  // basic match predicate
  def matches(f: Token => Boolean): Predicate = new Predicate {
    def check(t: Token): Boolean = f(t)
  }

  // either predicate
  def either(p1: Predicate, p2: Predicate): Predicate = new Predicate {
    def check(t: Token): Boolean = p1.check(t) || p2.check(t)
  }

  // repeat predicate n times except stop condition is met
  def repeat(p: Predicate, n: UInt = 0, stopl: Predicate = null): Predicate =
    new Predicate {
      def check(t: Token): Boolean = {
        if (n != 0) {
          boundary {
            var count: UInt = 0
            for (i <- 1 to n) {
              if (p.check(t)) count += 1 else boundary.break(false)
            }
            count >= n
          }
        } else if (stopl != null) {
          boundary {
            var count: UInt = 0
            while (!stopl.check(t)) {
              if (p.check(t)) count += 1 else boundary.break(false)
            }
            true
          }
        } else {
          throw new IllegalArgumentException(
            "repeat count and stop condition cannot both be 0"
          )
        }
      }
    }
}
