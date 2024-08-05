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
package ai.privado.languageEngine.go.tagger.source

import ai.privado.languageEngine.go.GoTestBase
import ai.privado.model.*
import ai.privado.traversal.TraversalValidator
import io.shiftleft.semanticcpg.language.*

class GoIdentifierTaggingTest extends GoTestBase with TraversalValidator {

  "Tagging derived sources" should {
    val (cpg, _) = code("""
        package main
        |
        |import "fmt"
        |
        |type User struct {
        |	FirstName string
        |	Age       int
        |	Location  string
        |	Email     string // 10
        |	passwd    string // 11 Note: this field is unexported
        |}
        |
        |func main() {
        |	// Creating a user instance
        |	user := User{
        |		FirstName: "John Doe",
        |		Age:       25,
        |		Location:  "New York",
        |		Email:     "abc@gmail.com", //20
        |		passwd:    "yourPassword", //21
        |	}
        |}
        |""".stripMargin)

    "tag member in a structure" in {
      val identifierNodes = cpg.member("FirstName").tag.nameExact(Constants.id).l
      identifierNodes.size shouldBe 1
      identifierNodes.value.head shouldBe "Data.Sensitive.PersonalIdentification.FirstName"
    }

    "tag user(of type structure) object" in {
      val List(userIdentifier) = cpg.identifier("user").lineNumber(16).l
      userIdentifier.tag
        .where(_.nameExact(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString))
        .value
        .head shouldBe "Data.Sensitive.PersonalIdentification.FirstName"
      userIdentifier.tag.where(_.nameExact(Constants.id)).size shouldBe 1
      userIdentifier.tag.where(_.nameExact(Constants.catLevelOne)).value.head shouldBe "DerivedSources"
    }

    "tag ded variable in a structure" in {
      val identifierNodes = cpg.member("passwd").tag.nameExact(Constants.id).l
      identifierNodes.size shouldBe 1
      identifierNodes.value.head shouldBe "Data.Sensitive.AccountData.AccountPassword"
    }

    "tag user(of type structure) object using DED rule" in {
      val List(userIdentifier) = cpg.identifier("user").lineNumber(16).l
      userIdentifier.tag
        .where(_.nameExact(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString))
        .value
        .l shouldBe List(
        "Data.Sensitive.PersonalIdentification.FirstName",
        "Data.Sensitive.AccountData.AccountPassword",
        "Data.Sensitive.ContactData.EmailAddress"
      )
      userIdentifier.tag.where(_.nameExact(Constants.id)).size shouldBe 1
      userIdentifier.tag.where(_.nameExact(Constants.catLevelOne)).value.head shouldBe "DerivedSources"
    }

    "build correct edges between derived and original sources" in {
      val List(userIdentifier)  = cpg.identifier("user").lineNumber(16).l
      val List(firstNameMember) = cpg.member("FirstName").l
      originalSourceTraversalValidator(userIdentifier, "Data.Sensitive.PersonalIdentification.FirstName")
      derivedSourceTraversalValidator(firstNameMember)
    }
  }
}
