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
import better.files.File
import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}

class SemanticFirstLevelDerivationTest extends JavaTaggingTestBase {

  val privadoScanConfig: PrivadoInput = PrivadoInput()
  var semantics: Semantics            = Semantics.empty
  var inputDirectory: File            = File.newTemporaryDirectory()

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
      |""".stripMargin

  "Semantic generated for 1st Level derivation" should {
    "have non-personal semantics for 2nd Level class by extends" in {
      semantics.elements.contains(FlowSemantic("User.getAmount:int()", List((0, 0)))) shouldBe true
      semantics.elements.contains(FlowSemantic("User.getId:java.lang.String()", List((0, 0)))) shouldBe true
    }

    "not have personal semantics for 2nd Level class by extends" in {
      semantics.elements.contains(FlowSemantic("User.getFirstName:java.lang.String()", List())) shouldBe false
    }

    "have non-personal semantics for 1st Level class" in {
      semantics.elements.contains(FlowSemantic("BaseClass.getId:java.lang.String()", List((0, 0)))) shouldBe true
    }

    "not have personal semantics for 1st Level class" in {
      semantics.elements.contains(FlowSemantic("BaseClass.getFirstName:java.lang.String()", List())) shouldBe false
    }

    "have setters semantics for 2nd Level class by extends" in {
      semantics.elements.contains(FlowSemantic("User.setAmount:void(int)", List((0, 0), (1, 1)))) shouldBe true
      semantics.elements.contains(FlowSemantic("User.setId:void(java.lang.String)", List((0, 0), (1, 1)))) shouldBe true
      semantics.elements.contains(
        FlowSemantic("User.setFirstName:void(java.lang.String)", List((0, 0), (1, 0), (1, 1)))
      ) shouldBe true
    }

    "have setters semantics for 1st Level class" in {
      semantics.elements.contains(
        FlowSemantic("BaseClass.setId:void(java.lang.String)", List((0, 0), (1, 1)))
      ) shouldBe true
      semantics.elements.contains(
        FlowSemantic("BaseClass.setFirstName:void(java.lang.String)", List((0, 0), (1, 0), (1, 1)))
      ) shouldBe true
    }

  }
}
