package ai.privado.tagger.sink.api

import ai.privado.model.{CatLevelOne, Constants}
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import ai.privado.model.NodeType
import org.scalatest.Assertion

trait APIValidator extends Matchers {

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
