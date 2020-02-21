package org.sqldap.parser

import org.parboiled2.{CharPredicate, Parser, ParserInput}

import scala.collection.immutable.ArraySeq

case class SelectStruct(fields: ArraySeq[String], table: String, attributes: Seq[(String, String)])

//noinspection TypeAnnotation since IntelliJ IDEA goes mad from lack of implicits on return types
class SqlParser(val input: ParserInput) extends Parser {
  protected[parser] def space = rule { oneOrMore(atomic(" ")) }

  protected[parser] def maybeSpace = rule { zeroOrMore(atomic(" ")) }

  protected[parser] def chars = rule { oneOrMore(CharPredicate.AlphaNum)}

  protected[parser] def comma = rule { maybeSpace ~ atomic(",") ~ maybeSpace }

  protected[parser] def fieldCondition = rule {
    zeroOrMore(atomic("and" | "AND" ) ~ space) ~ capture(chars) ~ maybeSpace ~ atomic("=") ~ maybeSpace ~ capture(chars) ~> { (key: String, value: String) => Tuple2(key, value) }
  }

  protected[parser] def select = rule {
    atomic("SELECT" | "select") ~ space ~ capture(oneOrMore(CharPredicate.AlphaNum).separatedBy(comma)) ~> { fields: String => ArraySeq.from(fields.replace(" ", "").split(",")) }
  }

  protected[parser] def from = rule {
    atomic("FROM" | "from") ~ space ~ capture(chars) ~> { table: String => table }
  }

  protected[parser] def where = rule {
    atomic("WHERE" | "where") ~ space ~ oneOrMore(fieldCondition).separatedBy(space) ~> { attributes: Seq[(String, String)] => attributes }
  }

  def selectStatement = rule {
    select ~ space ~ from ~ space ~ where ~ atomic(";") ~> { (fields: ArraySeq[String], table: String, attributes: Seq[(String, String)]) => SelectStruct(fields, table, attributes)}
  }
}
