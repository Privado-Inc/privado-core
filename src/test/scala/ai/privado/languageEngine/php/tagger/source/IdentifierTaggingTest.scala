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
package ai.privado.languageEngine.php.tagger.source

import ai.privado.model.*
import ai.privado.testfixtures.PhpFrontendTestSuite
import ai.privado.traversal.TraversalValidator
import io.shiftleft.semanticcpg.language.*

class IdentifierTaggingTest extends PhpFrontendTestSuite with TraversalValidator {
  "Tagging derived sources" should {
    val cpg = code(
      """
        |<?php
        |
        |  class User {
        |    public $firstName;
        |    public $lastName;
        |    public $age;
        |    public $email;
        |    public $dob;
        |
        |    function __construct($fname, $lname, $userAge, $userEmail, $userDob) {
        |      $this->firstName = $fname;
        |      $this->lastName = $lname;
        |      $this->age = $userAge;
        |      $this->email = $userEmail;
        |      $this->dob = $userDob;
        |    }
        |  }
        |
        |  $user = new User("a", "b", 1, "c@d.com", "01-01-90");
        |  echo $user->firstName;
        |?>
        |
        |""".stripMargin,
      "Test.php"
    )

    "tag member in a structure" in {
      cpg.member("firstName").tag.nameExact(Constants.id).value.l shouldBe List("Data.Sensitive.FirstName")

      cpg.member("dob").tag.nameExact(Constants.id).value.l shouldBe List(
        "Data.Sensitive.PersonalIdentification.DateofBirth"
      )
    }

    "be tagged as part of identifier tagger" in {
      val userObj = cpg.identifier("user").lineNumber(20).l
      userObj.tag
        .where(_.nameExact(InternalTag.OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME.toString))
        .value
        .head shouldBe "Data.Sensitive.FirstName"
      userObj.tag.where(_.nameExact(Constants.id)).size shouldBe 1
      userObj.tag.where(_.nameExact(Constants.catLevelOne)).value.l shouldBe List(CatLevelOne.DERIVED_SOURCES.name)
    }

    "build correct edges between derived and original sources" in {
      val List(userIdentifier)  = cpg.identifier("user").lineNumber(20).l
      val List(firstNameMember) = cpg.member("firstName").l
      originalSourceTraversalValidator(userIdentifier, "Data.Sensitive.FirstName")
      derivedSourceTraversalValidator(firstNameMember, userIdentifier)
    }

  }
}
