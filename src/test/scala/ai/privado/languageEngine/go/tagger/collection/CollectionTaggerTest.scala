package ai.privado.languageEngine.go.tagger.collection

import ai.privado.languageEngine.go.tagger.GoTaggingTestBase

class CollectionTaggerTest extends GoTaggingTestBase{

  override def beforeAll(): Unit = {
    new CollectionTagger(cpg, ruleCache).createAndApply()
  }

  override val goFileContents: String =
    """
      |package main
      |
      |func main() {
      | r.Get("/user/", getUser)
      |}
      |func getUser(firstName String){
      | return firstName
      |}
      |""".stripMargin

    "check"

}
