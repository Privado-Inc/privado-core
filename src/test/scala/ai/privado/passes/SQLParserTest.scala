/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 *
 */

package ai.privado.passes

import ai.privado.cache.RuleCache
import ai.privado.model.sql.SQLQueryType
import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import ai.privado.semantic.Language._
import io.shiftleft.semanticcpg.language._

class SQLParserTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "SQL parser" should {
    val cpg = code("""
        |select firstName, lastName from customer;
        |""".stripMargin)
    "create sqlColumn node" in {
      cpg.sqlColumn.size shouldBe 2
      cpg.sqlColumn.name.l shouldBe List("firstName", "lastName")
    }

    "create sqlTable node" in {
      cpg.sqlTable.size shouldBe 1
      cpg.sqlTable.name.l shouldBe List("customer")
    }

    "create sqlQuery node" in {
      cpg.sqlQuery.size shouldBe 1
      cpg.sqlQuery.name.l shouldBe List(SQLQueryType.SELECT)
      cpg.sqlQuery.code.head shouldBe "select firstName, lastName from customer;"
    }
    cpg.close()
  }

  def code(code: String): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    (inputDir / "sample.sql").write(code)
    val outputFile = File.newTemporaryFile()
    val config     = Config(inputPath = inputDir.toString(), outputPath = outputFile.toString())
    X2Cpg
      .withNewEmptyCpg(outputFile.toString(), config) { (cpg, config) =>
        new SQLParser(cpg, config.inputPath, new RuleCache()).createAndApply()
      }
      .get
  }
}
