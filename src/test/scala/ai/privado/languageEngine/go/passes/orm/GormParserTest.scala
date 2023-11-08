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
package ai.privado.languageEngine.go.passes.orm

import ai.privado.languageEngine.go.tagger.GoTaggingTestBase
import ai.privado.model.*
import ai.privado.semantic.Language.*
import io.joern.x2cpg.X2Cpg
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}

class GormParserTest extends GoTaggingTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    X2Cpg.applyDefaultOverlays(cpg)
    new GormParser(cpg).createAndApply()
  }

  override val goFileContents =
    """
      package models
      |
      |import (
      |	"github.com/jinzhu/gorm"
      |)
      |
      |// User Model
      |type User struct {
      |	// we need to use our own custom model because the id is different of the "gorm.Model"
      |	ID      string `gorm:"PRIMARY KEY; UNIQUE" json:"id"`
      |	Name    string `gorm:"type:varchar(255); NOT NULL" json:"name" validate:"required"`
      |	Email   string `gorm:"type:varchar(255)" json:"email"`
      |	Phone   string `gorm:"type:varchar(100); NOT NULL; UNIQUE; UNIQUE_INDEX" json:"phone" validate:"required"`
      |	Address string `gorm:"type:text" json:"address"`
      |	MyModel
      |}
      |
      |// Check if Users Model implements/match Model interface
      |var _ Model = &User{}
      |
      |// Save saves a User inside the database
      |func (u *User) Save(db *gorm.DB) error {
      |	uuidResult, err := uuid.NewRandom()
      |	if err != nil {
      |		log.Fatal(err)
      |	}
      |
      |	u.ID = uuidResult.String()
      |	u.CreatedAt = time.Now().String()
      |	u.UpdatedAt = time.Now().String()
      |
      |	err = db.Save(&u).Error
      |	if err != nil {
      |		return err
      |	}
      |	return nil
      |}
      |""".stripMargin

  "Adding sql nodes for GORM framework" should {
    "check table nodes" in {
      val tableNodes = cpg.sqlTable.l
      tableNodes.size shouldBe 1
      tableNodes.head.name shouldBe "User"
    }

    "check query nodes" in {
      val queryNodes = cpg.sqlQuery.l
      queryNodes.size shouldBe 1
      queryNodes.head.name shouldBe "CREATE"
    }

    "check column nodes" in {
      val columnNodes = cpg.sqlColumn.l
      columnNodes.size shouldBe 5

      val List(id, name, email, phone, address) = cpg.sqlColumn.l
      id.code shouldBe "ID"
      name.code shouldBe "Name"
      email.code shouldBe "Email"
      phone.code shouldBe "Phone"
      address.code shouldBe "Address"
    }

  }

}
