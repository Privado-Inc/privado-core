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

package ai.privado.languageEngine.java.tagger.source

import ai.privado.languageEngine.java.JavaTaggingTestBase
import ai.privado.model._
import io.shiftleft.semanticcpg.language._

class JavaIdentifierTaggingTest extends JavaTaggingTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
  }

  override val javaFileContents =
    """
      |public class User {
      |   public String firstName;
      |
      |   public String getFirstName() {return firstName;}
      |   public void setFirstName(String firstName) {this.firstName = firstName;}
      |}
      |""".stripMargin

  "Identifier Tagger" should {
    "tag a firstName identifier" in {
      val identifierNodes = cpg.identifier("firstName").tag.nameExact(Constants.id).l
      identifierNodes.size shouldBe 1
      identifierNodes.value.head shouldBe "Data.Sensitive.FirstName"
    }

    "tag fieldAccess of firstName" in {
      // Note - this test is Fails with the current query on cpg.method.callIn, but works fine on cpg.call
      val identifierNodes = cpg.fieldAccess.tag.nameExact(Constants.id).l
      identifierNodes.size shouldBe 2
      identifierNodes.value.head shouldBe "Data.Sensitive.FirstName"
    }
  }
}
