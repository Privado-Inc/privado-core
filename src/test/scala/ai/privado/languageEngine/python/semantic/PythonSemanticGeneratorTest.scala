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

package ai.privado.languageEngine.python.semantic
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import ai.privado.languageEngine.python.semantic.PythonSemanticGenerator.generateSemanticForTaint
import ai.privado.languageEngine.python.PythonTestUtility.code
import io.shiftleft.semanticcpg.language._

class PythonSemanticGeneratorTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "Python semantic generator" should {
    val cpg = code("""
        |import requests
        |def foo():
        |    orderId = "Mysource"
        |    item = orderId
        |    notMyItem = "something"
        |    response = requests.post(
        |            url="https://rest.marketingcloudapis.com/data/v1/async",
        |            body=[notMyItem, item]
        |        )
        |    print(response)
        |    myObject.foo()
        |""".stripMargin)

    "generate semantic for named argument" in {
      generateSemanticForTaint(
        cpg.call("post").head,
        -1
      ).flow shouldBe "0->-1 0->0 1->-1 1->1 2->-1 2->2 3->-1 3->3 \"body\"->\"body\" \"body\"->-1 \"url\"->\"url\" \"url\"->-1"
    }

    "generate semantic for non-named argument" in {
      generateSemanticForTaint(cpg.call("print").head, -1).flow shouldBe "0->-1 0->0 1->-1 1->1"
    }

    "generate semantic for function with 0 actual argument" in {
      generateSemanticForTaint(cpg.call("foo").head, -1).flow shouldBe "0->-1 0->0 1->-1 1->1"
    }
  }
}
