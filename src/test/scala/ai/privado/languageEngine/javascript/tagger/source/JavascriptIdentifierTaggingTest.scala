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

package ai.privado.languageEngine.javascript.tagger.source

import ai.privado.languageEngine.javascript.JavascriptTaggingTestBase
import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, Language, NodeType, RuleInfo}
import io.shiftleft.semanticcpg.language._

class JavascriptIdentifierTaggingTest extends JavascriptTaggingTestBase {
  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg).createAndApply()
  }
  override val javascriptFileContents: String =
    """
      |var firstName = "rahul"
      |
      |class User {
      | constructor(firstName) {
      |    this.firstName = firstName
      | }
      |
      | setFirstName(fName) {
      |   this.firstName = fName
      | }
      |}
      |""".stripMargin

  val sources = List(
    RuleInfo(
      "Data.Sensitive.FirstName",
      "FirstName",
      "",
      Array(),
      List("(?i).*firstName.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.JAVASCRIPT,
      Array()
    )
  )
  override val rule: ConfigAndRules =
    ConfigAndRules(sources, List(), List(), List(), List(), List(), List(), List(), List())

  "Identifier Tagger" should {
    "tag a firstName identifier" in {
      val identifierNodes = cpg.identifier.tag.nameExact(Constants.id).l
      identifierNodes.size shouldBe 2
      identifierNodes.value.head shouldBe "Data.Sensitive.FirstName"
    }

    "tag a firstName field access" in {
      val identifierNodes = cpg.fieldAccess.tag.nameExact(Constants.id).l
      identifierNodes.size shouldBe 2
      identifierNodes.value.head shouldBe "Data.Sensitive.FirstName"
    }
  }
  override val packageJsonFileContents: String = ""
}
