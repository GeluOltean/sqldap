package org.sqldap.parser

import org.parboiled2.{CharPredicate, Parser, ParserInput}

import scala.collection.immutable.ArraySeq

sealed trait ParsedStruct
case class SelectStruct(fields: ArraySeq[String], table: String, attributes: Seq[(String, String)]) extends ParsedStruct
case class DeleteStruct(table: String, attributes: Seq[(String, String)]) extends ParsedStruct
case class UpdateStruct(table: String, setAttributes: Seq[(String, String)], attributes: Seq[(String, String)]) extends ParsedStruct
case class InsertStruct(table: String, columnsToValues: Seq[(String, String)]) extends ParsedStruct

//noinspection TypeAnnotation since IntelliJ IDEA goes mad from lack of implicits on return types
class SqlParser(val input: ParserInput) extends Parser {
  protected[parser] def space = rule { oneOrMore(atomic(" ")) }

  protected[parser] def maybeSpace = rule { zeroOrMore(atomic(" ")) }

  protected[parser] def chars = rule { oneOrMore(CharPredicate.AlphaNum)}

  protected[parser] def comma = rule { maybeSpace ~ atomic(",") ~ maybeSpace }

  protected[parser] def assignment = rule {
    capture(chars) ~ maybeSpace ~ atomic("=") ~ maybeSpace ~ capture(chars) ~> { (key: String, value: String) => Tuple2(key, value) }
  }

  protected[parser] def valuesInParens = rule {
    atomic("(") ~ capture(oneOrMore(chars).separatedBy(comma)) ~ atomic(")") ~> { entry => ArraySeq.from(entry.replace(" ", "").split(",")) }
  }

  protected[parser] def fieldCondition = rule {
    zeroOrMore(atomic("and" | "AND" ) ~ space) ~ assignment
  }

  protected[parser] def set = rule {
    atomic("SET" | "set") ~ space ~ oneOrMore(assignment).separatedBy(comma)
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

  def deleteStatement = rule {
    atomic("DELETE" | "delete") ~ space ~ from ~ space ~ where ~ atomic(";") ~> { (table: String, attributes: Seq[(String, String)]) => DeleteStruct(table, attributes)}
  }

  def updateStatement = rule {
    atomic("UPDATE" | "update") ~ space ~ capture(chars) ~ space ~ set ~ space ~ where ~ atomic(";") ~> { (table: String, setAttributes: Seq[(String, String)], attributes: Seq[(String, String)]) => UpdateStruct(table, setAttributes, attributes)}
  }

  def insertStatement = rule {
    atomic("INSERT" | "insert") ~ space ~ atomic("INTO" | "into") ~ space ~ capture(chars) ~ maybeSpace ~ valuesInParens ~ space ~ atomic("VALUES" | "values") ~ space ~valuesInParens ~ atomic(";") ~> { (table: String, columns: Seq[String], values: Seq[String]) => InsertStruct(table, columns.zip(values)) }
  }
}
