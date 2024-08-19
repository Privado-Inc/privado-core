package ai.privado.passes

import ai.privado.cache.RuleCache
import ai.privado.semantic.language.*
import ai.privado.model.sql.SQLQueryType
import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.utility.PropertyParserPass
import ai.privado.model.Language
import io.joern.console.cpgcreation.guessLanguage
import io.shiftleft.semanticcpg.language._

class SQLPropertyParserTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  "SQL Property parser" should {
    val cpg = code("""
        |query: select firstName, lastName, credit_card from customer;
        |""".stripMargin)
    "create sqlColumn node" in {
      cpg.sqlColumn.size shouldBe 3
      cpg.sqlColumn.name.l shouldBe List("firstName", "lastName", "credit_card")
    }

    "create sqlTable node" in {
      cpg.sqlTable.size shouldBe 1
      cpg.sqlTable.name.l shouldBe List("customer")
    }

    "create sqlQuery node" in {
      cpg.sqlQuery.size shouldBe 1
      cpg.sqlQuery.name.l shouldBe List(SQLQueryType.SELECT)
      cpg.sqlQuery.code.head shouldBe "select firstName, lastName, credit_card from customer;"
    }

    "line number of sql query in the yml file" in {
      cpg.property.lineNumber.l.head shouldBe 2
    }

    cpg.close()
  }

  def code(code: String): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    (inputDir / "sample.yml").write(code)
    val outputFile = File.newTemporaryFile()
    val config     = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)

    val detectedLanguage = guessLanguage(inputDir.toString()).getOrElse("") match {
      case "PYTHONSRC" => Language.PYTHON
      case "JAVASRC"   => Language.JAVA
      case "JSSRC"     => Language.JAVASCRIPT
      case _           => Language.JAVA
    }

    X2Cpg
      .withNewEmptyCpg(outputFile.toString(), config) { (cpg, config) =>
        new PropertyParserPass(cpg, config.inputPath, new RuleCache, detectedLanguage).createAndApply()
        new SQLPropertyPass(cpg, config.inputPath, new RuleCache()).createAndApply()
      }
      .get
  }

}
