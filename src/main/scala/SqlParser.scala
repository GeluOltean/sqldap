import org.parboiled2.{CharPredicate, Parser, ParserInput}

import scala.collection.immutable.ArraySeq

//noinspection TypeAnnotation since IntelliJ IDEA goes mad from lack of implicits on return types
class SqlParser(val input: ParserInput) extends Parser {
  protected def space = rule { oneOrMore(atomic(" ")) }

  protected def maybeSpace = rule { zeroOrMore(atomic(" ")) }

  protected def chars = rule { oneOrMore(CharPredicate.AlphaNum)}

  protected def comma = rule { maybeSpace ~ atomic(",") ~ maybeSpace }

  protected def fieldCondition = rule {
    zeroOrMore(atomic("and" | "AND" ) ~ space) ~ capture(chars) ~ maybeSpace ~ atomic("=") ~ maybeSpace ~ capture(chars) ~> { (key: String, value: String) => Tuple2(key, value) }
  }

  protected def select = rule {
    atomic("SELECT" | "select") ~ space ~ capture(oneOrMore(CharPredicate.AlphaNum).separatedBy(comma)) ~> { fields: String => ArraySeq.from(fields.replace(" ", "").split(",")) }
  }

  protected def from = rule {
    atomic("FROM" | "from") ~ space ~ capture(chars) ~> { table: String => table }
  }

  protected def where = rule {
    atomic("WHERE" | "where") ~ space ~ oneOrMore(fieldCondition).separatedBy(space) ~> { attributes: Seq[(String, String)] => attributes }
  }

  def selectStatement = rule {
    select ~ space ~ where ~ atomic(";") ~> { (fields: ArraySeq[String], attributes: Seq[(String, String)]) => (fields, attributes)}
  }
}
