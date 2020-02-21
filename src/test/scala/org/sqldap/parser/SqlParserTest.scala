package org.sqldap.parser

import org.scalatest.TryValues
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.ArraySeq
import scala.util.Try

class SqlParserTest extends AnyFlatSpec with Matchers with TryValues {
  val fields: ArraySeq[String] = ArraySeq("e","f","g")
  val fieldsFormatted: String = fields.reduce((x, y) => f"$x,$y")
  val table: String = "table"
  val attributes: Seq[(String, String)] = Seq(("a", "A"), ("b", "B"))
  val attributesFormatted: String = attributes.map(tup => f"${tup._1} = ${tup._2}").reduce((x, y) => f"$x AND $y")

  val expectedSelect: SelectStruct = SelectStruct(fields, table, attributes)
  val expectedDelete: DeleteStruct = DeleteStruct(table, attributes)

  "Select" should "parse the selected fields" in {
    val parseResult: Try[ArraySeq[String]] = new SqlParser(f"SELECT $fieldsFormatted")
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
    val parseResult: Try[Seq[(String, String)]] = new SqlParser(f"WHERE $attributesFormatted")
      .where
      .run()
    parseResult.success.value should equal(attributes)
    val parseResult2: Try[Seq[(String, String)]] = new SqlParser("WHERE x = y")
      .where
      .run()
    parseResult2.success.value should equal(Seq(("x", "y")))
  }

  "Set" should "parse the attributes" in {
    val parseResult = new SqlParser(f"SET ${attributes.map(tup=>f"${tup._1}=${tup._2}").reduce((x, y) => f"$x, $y")}")
      .set
      .run()
    parseResult.success.value should equal(attributes)
  }

  "The select statement" should "parse correctly into SelectStruct" in {
    val parseResult: Try[SelectStruct] = new SqlParser(f"SELECT $fieldsFormatted FROM $table WHERE $attributesFormatted;")
      .selectStatement
      .run()
    parseResult.success.value should equal(expectedSelect)
  }

  "The delete statement" should "parse correctly into DeleteStruct" in {
    val parseResult: Try[DeleteStruct] = new SqlParser(f"DELETE FROM $table WHERE $attributesFormatted;")
      .deleteStatement
      .run()
    parseResult.success.value should equal(expectedDelete)
  }
}
