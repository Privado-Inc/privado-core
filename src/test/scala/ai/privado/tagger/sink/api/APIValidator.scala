package ai.privado.tagger.sink.api

import ai.privado.model.{CatLevelOne, Constants, NodeType, SystemConfig}
import ai.privado.tagger.TaggerValidator
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.Assertion

/** Use this trait for all the API related validation
  */
trait APIValidator extends Matchers with TaggerValidator {

  // Keeping these rules here as there are mostly used with API Validation
  val apiIdentifier    = SystemConfig(Constants.apiIdentifier, "(?i).*url")
  val apiHttpLibraries = SystemConfig(Constants.apiHttpLibraries, "org.apache.http.*")
  val apiSinks = SystemConfig(
    Constants.apiSinks,
    "(?i)(?:url|client|openConnection|request|execute|newCall|load|host|access|fetch|get|getInputStream|getApod|getForObject|getForEntity|list|set|put|post|proceed|trace|patch|Path|send|sendAsync|remove|delete|write|read|assignment|provider|exchange|postForEntity|postForObject|call|createCall|createEndpoint|dispatch|invoke|newMessage|getInput|getOutput|getResponse|marshall|unmarshall|send|asyncSend)"
  )

  /** Asserts if the given callNode is an API sink node
    * @param callNode
    *   \- Expected API call node
    * @return
    */
  def assertAPISinkCall(callNode: Call): Assertion = {
    callNode.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name).size shouldBe 1
    callNode.tag.nameExact(Constants.nodeType).valueExact(NodeType.API.toString).size shouldBe 1
  }

  /** Asserts if the given callNode have the tagged API endpoint as apiUrl
    * @param callNode
    *   \- API call node
    * @param apiUrl
    *   \- Expected API URL on the call node
    * @return
    */
  def assertAPIEndpointURL(callNode: Call, apiUrl: String): Assertion = {
    val domain = callNode.tag
      .nameExact("third_partiesapi")
      .value
      .headOption
      .getOrElse("")
      .stripPrefix(s"${Constants.thirdPartiesAPIRuleId}.")
    callNode.tag.nameExact(s"${Constants.apiUrl}${Constants.thirdPartiesAPIRuleId}.$domain").value.l shouldBe List(
      apiUrl
    )
  }

}
