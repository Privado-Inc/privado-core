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

package ai.privado.languageEngine.java

import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.semantic.SemanticGenerator
import ai.privado.languageEngine.java.tagger.source.{IdentifierTagger, InSensitiveCallTagger}
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}

class SemanticSecondLevelDerivationTest extends JavaTaggingTestBase {

  val privadoScanConfig: PrivadoInput = PrivadoInput(disable2ndLevelClosure = true)
  var semantics: Semantics            = Semantics.empty
  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg).createAndApply()
    new InSensitiveCallTagger(cpg).createAndApply()
    semantics = SemanticGenerator.getSemantics(cpg, privadoScanConfig)
  }
  override val javaFileContents =
    """
      |public class BaseClass {
      |   public String firstName;
      |   public String id;
      |   public String getFirstName() {return firstName;}
      |   public String getId() {return id;}
      |   public foo1() {
      |     BaseClass b = new BaseClass();
      |     b.getFirstName();
      |     b.getId();
      |   }
      |}
      |
      |
      |public class User extends BaseClass{
      |   public int amount;
      |   public int getAmount() {return amount;}
      |   public foo2() {
      |     User u = new User();
      |     u.getId();
      |     u.getAmount();
      |     u.getFirstName();
      |  }
      |}
      |
      |public class Customer {
      |   public String type;
      |   public BaseClass baseClass;
      |   public String getType() {return type;}
      |   public BaseClass getBaseClass() {return baseClass;}
      |   
      |   public foo3() {
      |     Customer c = new Customer();
      |     c.getType();
      |     c.getBaseClass();
      |   }
      |}
      |
      |
      |""".stripMargin

  "Semantic generated for 2nd Level derivation" should {
    "have non-personal semantics for 2nd Level class by extends" in {
      semantics.elements.contains(FlowSemantic("User.getAmount:int()", List())) shouldBe true
      semantics.elements.contains(FlowSemantic("User.getId:java.lang.String()", List())) shouldBe true
    }

    "not have personal semantics for 2nd Level class by extends" in {
      semantics.elements.contains(FlowSemantic("User.getFirstName:int()", List())) shouldBe false
    }

    "have non-personal semantics for 1st Level class" in {
      semantics.elements.contains(FlowSemantic("BaseClass.getId:java.lang.String()", List())) shouldBe true
    }

    "not have personal semantics for 1st Level class" in {
      semantics.elements.contains(FlowSemantic("BaseClass.getFirstName:java.lang.String()", List())) shouldBe false
    }

    "have non-personal semantics for 2nd Level class by member" in {
      semantics.elements.contains(FlowSemantic("Customer.getType:java.lang.String()", List())) shouldBe true
    }

    "not have personal semantics for 2nd Level class by member" in {
      semantics.elements.contains(FlowSemantic("Customer.getBaseClass:BaseClass", List())) shouldBe false
    }
  }
}
