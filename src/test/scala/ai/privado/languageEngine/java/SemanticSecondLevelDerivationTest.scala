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
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.languageEngine.java.tagger.source.{IdentifierTagger, InSensitiveCallTagger}
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}

class SemanticSecondLevelDerivationTest extends JavaTaggingTestBase {

  val privadoScanConfig: PrivadoInput = PrivadoInput(disable2ndLevelClosure = true)
  var semantics: Semantics            = Semantics.empty
  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    new InSensitiveCallTagger(cpg, ruleCache, taggerCache).createAndApply()
    semantics = JavaSemanticGenerator.getSemantics(cpg, privadoScanConfig, ruleCache)
  }
  override val javaFileContents =
    """
      |public class BaseClass {
      |   public String firstName;
      |   public String id;
      |   public String getFirstName() {return firstName;}
      |   public void setFirstName(String firstName) {this.firstName = firstName;} 
      |   public String getId() {return id;}
      |   public void setId(String id) {this.id = id;}
      |   public foo1() {
      |     BaseClass b = new BaseClass();
      |     b.setFirstName("Alex");
      |     b.setId("101");
      |     b.getFirstName();
      |     b.getId();
      |   }
      |}
      |
      |
      |public class User extends BaseClass{
      |   public int amount;
      |   public int getAmount() {return amount;}
      |   public void setAmount(int amount) {this.amount = amount;}
      |   public foo2() {
      |     User u = new User();
      |     u.setId("102");
      |     u.setAmount(25000);
      |     u.setFirstName("Hales");
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
      |   public void setType(String type) {this.type = type;}
      |   public void setBaseClass(BaseClass baseClass) {this.baseClass = baseClass;}
      |   
      |   public foo3() {
      |     Customer c = new Customer();
      |     c.setType("internal");
      |     c.setBaseClass(new BaseClass());
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

    "have setters semantics for 2nd Level class by extends" in {
      semantics.elements.contains(FlowSemantic("User.setAmount:void(int)", List())) shouldBe true
      semantics.elements.contains(FlowSemantic("User.setId:void(java.lang.String)", List())) shouldBe true
      semantics.elements.contains(
        FlowSemantic("User.setFirstName:void(java.lang.String)", List((0, 0), (1, 1), (1, 0)))
      ) shouldBe true
    }

    "have setters semantics for 1st Level class" in {
      semantics.elements.contains(FlowSemantic("BaseClass.setId:void(java.lang.String)", List())) shouldBe true
      semantics.elements.contains(
        FlowSemantic("BaseClass.setFirstName:void(java.lang.String)", List((0, 0), (1, 1), (1, 0)))
      ) shouldBe true
    }

    "have setters semantics for 2nd Level class by member" in {
      semantics.elements.contains(FlowSemantic("Customer.setType:void(java.lang.String)", List())) shouldBe true
      semantics.elements.contains(
        FlowSemantic("Customer.setBaseClass:void(BaseClass)", List((0, 0), (1, 1), (1, 0)))
      ) shouldBe true
    }
  }
}
