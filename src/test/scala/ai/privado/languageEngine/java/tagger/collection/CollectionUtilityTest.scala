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

package ai.privado.languageEngine.java.tagger.collection

import ai.privado.languageEngine.java.JavaTaggingTestBase
import io.shiftleft.semanticcpg.language._

class CollectionUtilityTest extends JavaTaggingTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()

  }

  override val javaFileContents: String =
    """
      |
      |@RequestMapping("/api/public/user")
      |public class AuthenticationService {
      |
      |
      | @PostMapping("/signup")
      |	public UserProfileD signup(@RequestBody String firstName) {
      | }
      |
      | @PostMapping(value = "/signin")
      |	public UserProfileD signin(@RequestBody String firstName) {
      | }
      |
      | @PostMapping
      |	public UserProfileD sample3(@RequestBody String firstName) {
      | }
      |
      |}
      |
      |
      |""".stripMargin

  "Get Url for annotation" should {
    "give url for sample1" in {
      CollectionUtility.getUrlFromAnnotation(cpg.typeDecl.annotation.head) shouldBe "/api/public/user"
    }
  }

  "Get Url for annotation" should {
    "give url for sample2" in {
      CollectionUtility.getUrlFromAnnotation(cpg.method("signup").annotation.head) shouldBe "/signup"
    }
  }

  "Get Url for annotation" should {
    "give url for sample3" in {
      CollectionUtility.getUrlFromAnnotation(cpg.method("sample3").annotation.head) shouldBe "sample3"
    }
  }
}
