package ai.privado.tagger.source

import ai.privado.model.Constants
import ai.privado.testfixtures.DefaultFrontendTestSuite
import ai.privado.semantic.language.Language.*
import io.shiftleft.semanticcpg.language.*

class SqlQueryTaggerTests extends DefaultFrontendTestSuite {

  "Sql Query tagger" should {
    val cpg = code(
      """
                     |select DISTINCT(firstName) As firstName,
                     |lastName from customer;
                     |""".stripMargin,
      "sample1.sql"
    )

    "tag nodes when source present in space split" in {
      val List(firstName, lastName) = cpg.sqlColumn.l
      firstName.tag.nameExact(Constants.id).value.l shouldBe List("Data.Sensitive.FirstName")
    }
  }
}
