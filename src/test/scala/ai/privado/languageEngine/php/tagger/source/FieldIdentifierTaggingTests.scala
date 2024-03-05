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

class FieldIdentifierTaggingTests extends PhpTestBase {
  "Field access in code" should {
    "be tagged as part of identifier tagger" in {
      val (cpg, _) = code("""
          |<?php
          |class Person {
          |  public $firstName;
          |
          |  function initialize() {
          |    add_phone("phone");
          |  }
          |
          |  function add_phone($ph) {
          |    //
          |  }
          |}
          |?>
          |""".stripMargin)

      val List(firstName) = cpg.member.l
      firstName.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.SOURCES.name)
    }
  }
}
