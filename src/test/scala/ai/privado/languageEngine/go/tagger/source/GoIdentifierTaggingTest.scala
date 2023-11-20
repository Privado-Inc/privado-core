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
import io.shiftleft.semanticcpg.language.*

class GoIdentifierTaggingTest extends GoTestBase {

  "Tagging derived sources" should {
    val (cpg, _) = code("""
        package main
        |
        |type User struct {
        |	FirstName     string
        |	Age      int
        |	Location string
        |	Email    string
        |}
        |
        |func main() {
        |	// Creating a user instance
        |	user := User{
        |		Name:     "John Doe",
        |		Age:      25,
        |		Location: "New York",
        |		Email: "abc@gmail.com",
        |	}
        |}
        |
        |""".stripMargin)

    "tag member in a structure" in {
      val identifierNodes = cpg.member("FirstName").tag.nameExact(Constants.id).l
      identifierNodes.size shouldBe 1
      identifierNodes.value.head shouldBe "Data.Sensitive.FirstName"
    }

    "tag user(of type structure) object" in {
      val List(userIdentifier) = cpg.identifier("user").lineNumber(13).l
      userIdentifier.tag
        .where(_.nameExact(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString))
        .value
        .head shouldBe "Data.Sensitive.FirstName"
      userIdentifier.tag.where(_.nameExact(Constants.id)).size shouldBe 1
      userIdentifier.tag.where(_.nameExact(Constants.catLevelOne)).value.head shouldBe "DerivedSources"
    }
  }
}
