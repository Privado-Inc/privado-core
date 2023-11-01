package ai.privado.exporter

import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.JavaTaggingTestBase
import ai.privado.languageEngine.java.tagger.source.IdentifierTagger
import ai.privado.model.{CatLevelOne, Constants}
import io.shiftleft.semanticcpg.language.*
class SourceExporterTest extends JavaTaggingTestBase {

  override def beforeAll(): Unit = {
    super.beforeAll()
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
  }

  override val javaFileContents =
    """
      |class User {
      |   public String firstName;
      |
      |   public String getFirstName() {return firstName;}
      |   public void setFirstName(String firstName) {this.firstName = firstName;}
      |}
      |
      |class Auth {
      |   public display(User user) {System.out.println(user);}
      |}
      |""".stripMargin

  "Identifier Tagger" should {
    "tag a derived source" in {
      val identifierNodes = cpg.identifier("user").l
      identifierNodes.size shouldBe 1
      identifierNodes.tag
        .nameExact(Constants.catLevelOne)
        .valueExact(CatLevelOne.DERIVED_SOURCES.name)
        .nonEmpty shouldBe true
    }
  }

  "Source exporter" should {
    "not export derived source under processing" in {
      val sourceExporter = SourceExporter(cpg, ruleCache, PrivadoInput(disableDeDuplication = true))
      !sourceExporter.getProcessing.flatMap(_.occurrences).map(_.sample).exists(_.equals("user")) shouldBe true
    }
  }
}
