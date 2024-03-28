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
import ai.privado.semantic.Language.*
import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable
class SQLParserTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {
  private val cpgs        = mutable.ArrayBuffer.empty[Cpg]
  private val outPutFiles = mutable.ArrayBuffer.empty[File]
  private val inputDirs   = mutable.ArrayBuffer.empty[File]

  "SQL parser" should {
    val cpg = code("""
        |select firstName,
        |lastName from customer;
        |""".stripMargin)
    "create sqlColumn node" in {
      cpg.sqlColumn.size shouldBe 2
      cpg.sqlColumn.name.l shouldBe List("firstName", "lastName")
      cpg.sqlColumn.lineNumber.l shouldBe List(2, 3)
    }

    "create sqlTable node" in {
      cpg.sqlTable.size shouldBe 1
      cpg.sqlTable.name.l shouldBe List("customer")
    }

    "create sqlQuery node" in {
      cpg.sqlQuery.size shouldBe 1
      cpg.sqlQuery.name.l shouldBe List(SQLQueryType.SELECT)
      cpg.sqlQuery.code.head shouldBe
        """select firstName,
          |lastName from customer;""".stripMargin
    }
  }

  "CREATE SQL query" should {
    val cpg = code("""
        |CREATE TABLE IF NOT EXISTS votes (
        |		id SERIAL NOT NULL,
        |		created_at datetime NOT NULL,
        |		candidate VARCHAR(6) NOT NULL,
        |		PRIMARY KEY (id)
        |	);
        |
        |SELECT id, candidate from votes;
        |""".stripMargin)
    "Query node check" in {
      cpg.sqlQuery.size shouldBe 2
      val List(a, b) = cpg.sqlQuery.l
      a.name shouldBe SQLQueryType.CREATE
      b.name shouldBe SQLQueryType.SELECT
    }

    "SqlTable node check" in {
      val List(x, y) = cpg.sqlTable.l
      x.name shouldBe "votes"
      x.lineNumber shouldBe Some(2)

      y.name shouldBe "votes"
      y.lineNumber shouldBe Some(9)
    }

    "Traversal from query to table" in {
      val List(x, y) = cpg.sqlQuery.sqlTable.l
      x.name shouldBe "votes"
      x.lineNumber shouldBe Some(2)

      y.name shouldBe "votes"
      y.lineNumber shouldBe Some(9)
    }

    "Traversal from table to column" in {
      val List(id, created_at, candidate) = cpg.sqlQuery.sqlTable.lineNumber(2).sqlColumn.l
      id.name shouldBe "id"
      created_at.name shouldBe "created_at"
      candidate.name shouldBe "candidate"

      val List(id1, candidate1) = cpg.sqlQuery.sqlTable.lineNumber(9).sqlColumn.l
      id1.name shouldBe "id"
      candidate1.name shouldBe "candidate"

      id1.lineNumber shouldBe Some(9)
      candidate1.lineNumber shouldBe Some(9)
    }

    "have correct attributes for members" in {
      val List(id, created_at, candidate) = cpg.sqlQuery.sqlTable.lineNumber(2).sqlColumn.l
      id.lineNumber shouldBe Some(3)
      created_at.lineNumber shouldBe Some(4)
      candidate.lineNumber shouldBe Some(5)
    }
  }

  "CREATE query with columns starting on same line as table definition" should {
    val cpg = code("""
      |CREATE TABLE IF NOT EXISTS votes(  id SERIAL NOT NULL,
      | created_at datetime NOT NULL,
      | candidate VARCHAR(6) NOT NULL,
      | PRIMARY KEY(id)
      |);""".stripMargin)

    "Query node check" in {
      cpg.sqlQuery.size shouldBe 1
      val List(a) = cpg.sqlQuery.l
      a.name shouldBe SQLQueryType.CREATE
    }

    "SqlTable node check" in {
      val List(x) = cpg.sqlTable.l
      x.name shouldBe "votes"
      x.lineNumber shouldBe Some(2)
    }

    "Traversal from query to table" in {
      val List(x) = cpg.sqlQuery.sqlTable.l
      x.name shouldBe "votes"
      x.lineNumber shouldBe Some(2)
    }

    "Traversal from table to column" in {
      val List(id, created_at, candidate) = cpg.sqlQuery.sqlTable.lineNumber(2).sqlColumn.l
      id.name shouldBe "id"
      created_at.name shouldBe "created_at"
      candidate.name shouldBe "candidate"
    }

    "have correct attributes for members" in {
      val List(id, created_at, candidate) = cpg.sqlQuery.sqlTable.lineNumber(2).sqlColumn.l
      id.lineNumber shouldBe Some(2)
      created_at.lineNumber shouldBe Some(3)
      candidate.lineNumber shouldBe Some(4)
    }
  }

  def code(code: String): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    (inputDir / "sample.sql").write(code)
    inputDirs.addOne(inputDir)
    val outputFile = File.newTemporaryFile()
    outPutFiles.addOne(outputFile)
    val config = Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
    val cpg = X2Cpg
      .withNewEmptyCpg(outputFile.toString(), config) { (cpg, config) =>
        new SQLParser(cpg, config.inputPath, new RuleCache()).createAndApply()
      }
      .get
    cpgs.addOne(cpg)
    cpg
  }

  override def afterAll(): Unit = {
    cpgs.foreach(_.close())
    outPutFiles.foreach(_.delete())
    inputDirs.foreach(_.delete())
    super.afterAll()
  }
}
