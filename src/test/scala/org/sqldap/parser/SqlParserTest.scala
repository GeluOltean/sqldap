package org.sqldap.parser

import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ArraySeq
import scala.util.Try

class SqlParserTest extends AnyFlatSpec with Matchers with TryValues {
  val fields: ArraySeq[String] = ArraySeq("e","f","g")
  val table: String = "table"
  val attributes: Seq[(String, String)] = Seq(("a", "A"), ("b", "B"))
  val expected: SelectStruct = SelectStruct(fields, table, attributes)

  "Select" should "parse the selected fields" in {
    val parseResult: Try[ArraySeq[String]] = new SqlParser(f"SELECT ${fields.reduce((x, y) => f"$x,$y")}")
      .select
      .run()
    parseResult.success.value should equal(fields)
  }

  "From" should "parse the table name" in {
    val parseResult: Try[String] = new SqlParser(f"FROM $table")
      .from
      .run()
    parseResult.success.value should equal(table)
  }

  "Where" should "parse the attributes" in {
    val parseResult: Try[Seq[(String, String)]] = new SqlParser(f"WHERE ${attributes.map(tup => f"${tup._1} = ${tup._2}").reduce((x, y) => f"$x AND $y")}")
      .where
      .run()
    parseResult.success.value should equal(attributes)
  }

  "The select statement" should "parse correctly into SelectStruct" in {
    val parseResult: Try[SelectStruct] = new SqlParser(f"SELECT ${fields.reduce((x, y) => f"$x,$y")} FROM $table " +
      f"WHERE ${attributes.map(tup => f"${tup._1} = ${tup._2}").reduce((x, y) => f"$x AND $y")}" +
      ";")
      .selectStatement
      .run()
    parseResult.success.value should equal(expected)
  }
}
