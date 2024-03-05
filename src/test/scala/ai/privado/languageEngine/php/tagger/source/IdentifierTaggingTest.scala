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

import ai.privado.languageEngine.php.PhpTestBase
import ai.privado.model.*
import io.shiftleft.semanticcpg.language.*

class IdentifierTaggingTest extends PhpTestBase {
  "Tagging derived sources" should {
    val (cpg, _) = code("""
        |<?php
        |class Person {
        |  public $firstName;
        |  public $lastName;
        |  public $dob;
        |  public $emailId;
        |  public $phoneNumber;
        |
        |  function set_fname($firstName) {
        |    $this->firstName = $firstName;
        |  }
        |
        |  function get_email() {
        |    return $emailId;
        |  }
        |}
        |?>
        |
        |""".stripMargin)

    "tag member in a structure" in {
      cpg.member("firstName").tag.nameExact(Constants.id).value.l shouldBe List(
        "Data.Sensitive.PersonalIdentification.FirstName"
      )

      cpg.member("dob").tag.nameExact(Constants.id).value.l shouldBe List(
        "Data.Sensitive.PersonalIdentification.DateofBirth"
      )
    }
  }
}
