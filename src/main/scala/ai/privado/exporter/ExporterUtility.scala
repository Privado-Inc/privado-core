package ai.privado.exporter

import ai.privado.cache.RuleCache
import ai.privado.model.Constants
import ai.privado.utility.Utilities.dump
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language.{DefaultNodeExtensionFinder, NodeExtensionFinder, toExtendedNode}

import scala.collection.mutable

object ExporterUtility {

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  /**
    Convert to path element schema object
   */
  def convertPathElement(nodes: List[CfgNode]): Seq[mutable.LinkedHashMap[String, Json]] = {
    def converter(node: CfgNode) = {
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
      occurrence
    }
    nodes.map(node => converter(node))
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

  def getPolicyInfoForExporting(policyId: String): mutable.Map[String, Json] = {
    val policyOutput = mutable.LinkedHashMap[String, Json]()
    RuleCache.getPolicy(policyId) match {
      case Some(policy) =>
        addToMap(policyOutput, Constants.description, policy.description)
        addToMap(policyOutput, Constants.action, policy.action.toString)
        if (policy.tags.nonEmpty) {
          policyOutput.addOne(Constants.tags -> policy.tags.asJson)
        }
        policyOutput
      case None => policyOutput
    }
  }

}
