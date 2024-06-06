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

import ai.privado.model.*
import ai.privado.testfixtures.JavaFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class JavaSourceTaggingTest extends JavaFrontendTestSuite {

  "Variable names matching with the source rules" should {
    val cpg = code("""
                     |public class User {
                     |   public String firstName;
                     |
                     |   public String getFirstName() {return firstName;}
                     |   public void setFirstName(String firstName) {this.firstName = firstName;}
                     |}
                     |""".stripMargin)

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

  "Simple use case of Member variables of class matching with source rules" should {
    val cpg = code("""
        |public class User {
        |   public String firstName;
        |
        |   public String getFirstName() {return firstName;}
        |   public void setFirstName(String firstName) {this.firstName = firstName;}
        |}
        |""".stripMargin)
      .moreCode("""
        |public class Test {
        |   public static void main(String[] args){
        |       User us = new User();
        |       System.out.println(us);
        |   }
        |}
        |""".stripMargin)

    "tag a us identifier" in {
      val identifierNodes = cpg.identifier.where(_.tag.nameExact("catLevelOne").valueExact("DerivedSources")).l
      // TODO: Ideally it should be 2 nodes at line no 4 and line 5. However Line no 4 Identifier node is getting tagged two times.
      // As it doesn't harm in processing, we will keep this issue on back burner.
      identifierNodes.size shouldBe 3
      identifierNodes.code.l shouldBe List("us", "us", "us")
      identifierNodes.lineNumber.l shouldBe List(4, 4, 5)
    }
  }

  "Templated use case of Derived source" should {
    val cpg = code("""
        |public class User {
        |   public String firstName;
        |
        |   public String getFirstName() {return firstName;}
        |   public void setFirstName(String firstName) {this.firstName = firstName;}
        |}
        |""".stripMargin)
      .moreCode("""
        |import java.util.Optional;
        |public class Test {
        |   public Optional<User> findUser() {
        |     User userone = new User();
        |     return Optional.ofNullable(userone);
        |   }
        |   public static void main(String[] args){
        |       Optional<User> userOption = findUser();
        |       User user = userOption.orElseThrow(() ->
        |            new RunTimeExceptionPlaceHolder("User doesn't exist!!")
        |       );
        |       System.out.println(user);
        |   }
        |}
        |""".stripMargin)

    "tag a userone and user identifier" in {
      val identifierNodes = cpg.identifier.where(_.tag.nameExact("catLevelOne").valueExact("DerivedSources")).l
      identifierNodes.size shouldBe 5
      identifierNodes.code.l shouldBe List("userone", "userone", "userone", "user", "user")
      identifierNodes.lineNumber.l shouldBe List(5, 5, 6, 10, 13)
    }
  }

  "Field access of an external class" should {
    val cpg = code("""
        |import ai.privado.User;
        |public class Test {
        |   public static void main(String[] args){
        |       User us = new User();
        |       String firstName = us.firstName; // As a field access is present we can infer the User class has firstName member
        |       System.out.println(us);
        |   }
        |}
        |""".stripMargin)

    "tag derive source" in {
      val identifierNodes = cpg.identifier.where(_.tag.nameExact("catLevelOne").valueExact("DerivedSources")).l
      identifierNodes.size shouldBe 4
      identifierNodes.code.l shouldBe List("us", "us", "us", "us")
      identifierNodes.lineNumber.l shouldBe List(5, 5, 6, 7)

      cpg.typeDecl.fullName(identifierNodes.head.typeFullName).isExternal.l shouldBe List(true)
    }
  }
}
