package ai.privado.tagger.sink.api

import ai.privado.exporter.ExporterUtility
import ai.privado.model.{
  CatLevelOne,
  Constants,
  DataFlowPathModel,
  FilterProperty,
  Language,
  NodeType,
  RuleInfo,
  SourceCodeModel
}
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.Assertion
import ai.privado.cache.{
  AppCache,
  AuditCache,
  DataFlowCache,
  DatabaseDetailsCache,
  PropertyFilterCache,
  S3DatabaseDetailsCache,
  TaggerCache
}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.model.Constants.outputFileName
import ai.privado.rule.RuleInfoTestData
import ai.privado.rule.RuleInfoTestData.ruleCache
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg

trait CollectionValidator extends Matchers {

  /** Asserts if the given methodNode is a collection sink node
    * @param methodNode
    *   \- Expected collection method node
    * @return
    */
  def assertCollectionMethod(methodNode: Method): Assertion = {
    methodNode.tag.nameExact(Constants.catLevelOne).value.l shouldBe List(CatLevelOne.COLLECTIONS.name)
  }

  /** Asserts if the given methodNode has the tagged endpoint as the collection method endpoint
    * @param methodNode
    *   \- Collection method node
    * @param collectionEndpoint
    *   \- Expected endpoint URL on the method node
    * @return
    */
  def assertCollectionUrl(methodNode: Method, collectionEndpoint: String): Assertion = {
    methodNode.tag.name("COLLECTION_METHOD_ENDPOINT").value.l shouldBe List(collectionEndpoint)
  }

  /** Asserts if the privado.json contains the tagged collections.
    *
    * @param cpg
    *   \- Cpg generated after the scan
    * @param expectedCollections
    *   \- Expected number of collections to be asserted
    * @return
    */
  def assertCollectionInFinalJson(cpg: Cpg, expectedCollections: Int): Assertion = {
    val privadoInput = PrivadoInput()
    val appCache     = new AppCache()
    appCache.repoLanguage = Language.PHP

    val (_, _, _, _, _, collections, _) = ExporterUtility.generateIndividualComponent(
      cpg,
      outputFileName,
      "temporaryPhpPath",
      Map.empty[String, Path],
      ruleCache,
      new TaggerCache(),
      List.empty[DataFlowPathModel],
      privadoInput,
      new S3DatabaseDetailsCache(),
      None,
      appCache,
      new DatabaseDetailsCache(),
      new PropertyFilterCache(),
      new DataFlowCache(privadoInput, AuditCache())
    )

    collections.size shouldBe 1
  }

}
