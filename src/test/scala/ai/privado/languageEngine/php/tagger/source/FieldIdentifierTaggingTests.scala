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

import ai.privado.cache.RuleCache
import ai.privado.model.*
import ai.privado.rule.RuleInfoTestData
import ai.privado.testfixtures.PhpFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class FieldIdentifierTaggingTests extends PhpFrontendTestSuite {
  val configAndRules: ConfigAndRules = ConfigAndRules(sources = RuleInfoTestData.sourceRule)
  val ruleCache: RuleCache           = new RuleCache().setRule(configAndRules)

  "Field access in code" should {
    val cpg = code(
      """
          |<?php
          |class Person {
          |  public $firstName;
          |
          |  function initialize() {
          |    $this->firstName = "John";
          |    add_phone("phone");
          |  }
          |
          |  function add_phone($ph) {
          |    //
          |  }
          |}
          |?>
          |""".stripMargin,
      "Test.php"
    )
      .withRuleCache(ruleCache)

    "be tagged as part of identifier tagger" in {
      val List(firstNameField) = cpg.fieldAccess.l
      firstNameField.code shouldBe "$this->firstName"
      firstNameField.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.SOURCES.name)
    }
  }
}
