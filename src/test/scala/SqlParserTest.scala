import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues

import scala.collection.immutable.ArraySeq
import scala.util.Try

class SqlParserTest extends AnyFlatSpec with Matchers with TryValues {
  val fields: ArraySeq[String] = ArraySeq("e","f","g")
  val table: String = "table"
  val attributes: Seq[(String, String)] = Seq(("a", "A"), ("b", "B"))
  val expected: SelectStruct = SelectStruct(fields, table, attributes)

  "The select statement" should "parse correctly" in {
    val parseResult: Try[SelectStruct] = new SqlParser(f"SELECT ${fields.reduce((x, y) => f"$x,$y")} FROM $table " +
      f"WHERE ${attributes.map(tup => f"${tup._1} = ${tup._2}").reduce((x, y) => f"$x AND $y")}" +
      ";")
      .selectStatement
      .run()
    parseResult.success.value should equal(expected)
  }
}
