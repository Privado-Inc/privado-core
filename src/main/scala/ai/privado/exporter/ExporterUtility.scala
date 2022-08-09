package ai.privado.exporter

import ai.privado.cache.RuleCache
import ai.privado.model.Constants
import ai.privado.utility.Utilities.dump
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language.toExtendedNode
import ai.privado.semantic.Language._

import scala.collection.mutable

object ExporterUtility {

  /** Convert List of path element schema object
    */
  def convertPathElements(nodes: List[CfgNode]): Seq[mutable.LinkedHashMap[String, Json]] = {
    nodes.flatMap(node => convertIndividualPathElement(node))
  }

  /** Convert Individual path element
    * @param node
    * @return
    */
  def convertIndividualPathElement(node: CfgNode) = {
    val occurrence   = mutable.LinkedHashMap[String, Json]()
    val nodeLocation = node.location
    occurrence.addOne(Constants.sample -> nodeLocation.symbol.asJson)
    occurrence.addOne(Constants.lineNumber -> {
      nodeLocation.lineNumber match {
        case Some(n) => n.asJson
        case None    => Constants.minusOne.asJson
      }
    })
    occurrence.addOne(Constants.columnNumber -> {
      node.columnNumber match {
        case Some(n) => n.asJson
        case None    => Constants.minusOne.asJson
      }
    })
    occurrence.addOne(Constants.fileName -> nodeLocation.filename.asJson)

    occurrence.addOne(Constants.excerpt -> dump(nodeLocation.filename, node.lineNumber).asJson)
    if (nodeLocation.filename == "<empty>" || nodeLocation.symbol == "<empty>")
      None
    else
      Some(occurrence)
  }

  private def addToMap(outputMap: mutable.LinkedHashMap[String, Json], name: String, value: String) = {
    if (value.nonEmpty)
      outputMap.addOne(name -> value.asJson)
  }
  def getRuleInfoForExporting(ruleId: String): mutable.Map[String, Json] = {
    val ruleInfoOuput = mutable.LinkedHashMap[String, Json]()
    RuleCache.getRuleInfo(ruleId) match {
      case Some(rule) =>
        addToMap(ruleInfoOuput, Constants.id, rule.id)
        addToMap(ruleInfoOuput, Constants.name, rule.name)
        addToMap(ruleInfoOuput, Constants.category, rule.category)
        if (rule.domains.nonEmpty)
          ruleInfoOuput.addOne(Constants.domains -> rule.domains.asJson)
        addToMap(ruleInfoOuput, Constants.sensitivity, rule.sensitivity)
        ruleInfoOuput.addOne(Constants.isSensitive -> rule.isSensitive.asJson)
        if (rule.tags.nonEmpty)
          ruleInfoOuput.addOne(Constants.tags -> rule.tags.asJson)
        ruleInfoOuput
      case None => ruleInfoOuput
    }
  }

  def getPolicyInfoForExporting(policyOrThreatId: String): mutable.Map[String, Json] = {
    val policyOutput = mutable.LinkedHashMap[String, Json]()
    RuleCache.getPolicyOrThreat(policyOrThreatId) match {
      case Some(policyOrThreat) =>
        addToMap(policyOutput, Constants.name, policyOrThreat.name)
        addToMap(policyOutput, Constants.policyOrThreatType, policyOrThreat.policyOrThreatType.toString)
        addToMap(policyOutput, Constants.description, policyOrThreat.description)
        addToMap(policyOutput, Constants.fix, policyOrThreat.fix)
        if (policyOrThreat.action != null)
          addToMap(policyOutput, Constants.action, policyOrThreat.action.toString)
        if (policyOrThreat.tags.nonEmpty) {
          policyOutput.addOne(Constants.tags -> policyOrThreat.tags.asJson)
        }
        policyOutput
      case None => policyOutput
    }
  }

}
