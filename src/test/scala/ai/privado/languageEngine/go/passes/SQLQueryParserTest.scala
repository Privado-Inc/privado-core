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
package ai.privado.languageEngine.go.passes

import ai.privado.languageEngine.go.tagger.GoTaggingTestBase
import ai.privado.languageEngine.go.tagger.source.IdentifierTagger
import ai.privado.model.*
import ai.privado.semantic.Language._
import io.shiftleft.semanticcpg.language._
import ai.privado.languageEngine.go.passes.SQLQueryParser

class SQLQueryParserTest extends GoTaggingTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new SQLQueryParser(cpg).createAndApply()
  }

  override val goFileContents =
    """
      package main
      |
      |func migrateDB(db *sql.DB) error {
      |	createVotes := `CREATE TABLE IF NOT EXISTS votes (
      |		id SERIAL NOT NULL,
      |		created_at datetime NOT NULL,
      |		candidate VARCHAR(6) NOT NULL,
      |		PRIMARY KEY (id)
      |	);`
      |	_, err := db.Exec(createVotes)
      |	return err
      |}
      |
      |""".stripMargin

  "Check SQL nodes" should {
    "check column nodes" in {
      val columnNodes = cpg.sqlColumn.l
      columnNodes.size shouldBe 3
      val List(id, createdAt, candidate) = cpg.sqlColumn.l
      id.code shouldBe "id"
      createdAt.code shouldBe "created_at"
      candidate.code shouldBe "candidate"

    }

    "check table nodes" in {
      val tableNodes = cpg.sqlTable.l
      tableNodes.size shouldBe 1
    }

  }

}
