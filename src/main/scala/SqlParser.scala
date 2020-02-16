import org.parboiled2.{CharPredicate, Parser, ParserInput}

class SqlParser(val input: ParserInput) extends Parser {
  protected def space = rule { zeroOrMore(atomic(" ")) }

  protected def chars = rule { oneOrMore(CharPredicate.AlphaNum) }

  protected def comma = rule { space ~ atomic(",") ~ space }

  protected def fieldCondition = rule {
    capture(chars) ~ space ~ atomic("=") ~ space ~ capture(chars) ~> { (key, value) => (key, value) }
  }

  protected def select = rule {
    atomic("SELECT" | "select") ~ space ~ capture(oneOrMore(CharPredicate.AlphaNum).separatedBy(comma)) ~> {field => field}
  }

  protected def where = rule {
    atomic("WHERE" | "where") ~ oneOrMore(fieldCondition).separatedBy(comma) ~ space ~ atomic(";") ~ space ~> {conditions => conditions}
  }

  def selectStatement = rule {
    capture(select) ~ capture(where) ~> { (fields, attributes) => (fields, attributes) }
  }
}
