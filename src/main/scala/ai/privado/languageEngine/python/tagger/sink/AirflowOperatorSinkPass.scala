package ai.privado.languageEngine.python.tagger.sink

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.{CatLevelOne, FilterProperty, Language, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.model.Constants
import ai.privado.utility.Utilities.addRuleTags
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class AirflowOperatorSinkPass(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {

  private val AIRFLOW_CUSTOM_RULE_ID = "Airflow.Custom.Operator"
  override def generateParts(): Array[RuleInfo] = {
    (ruleCache.getRule.sinks.filter(rule => rule.id.contains("ThirdParties.Operator")) ++ List(
      getCustomOperatorTag
    )).toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    if (ruleInfo.id.equals(AIRFLOW_CUSTOM_RULE_ID)) {
      // Execute only for custom operator
      tagCustomOperator(builder, ruleInfo)
    } else {
      cpg.call
        .name(ruleInfo.combinedRulePattern)
        .foreach(node => {
          addRuleTags(builder, node, ruleInfo, ruleCache)
        })
    }
  }

  // Rule for Custom Operator
  private def getCustomOperatorTag: RuleInfo = {
    RuleInfo(
      AIRFLOW_CUSTOM_RULE_ID,
      "Airflow",
      "Third Parties",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("AirflowOperator"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      Constants.third_parties,
      Language.PYTHON,
      Array()
    )
  }

  private def tagCustomOperator(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val customOperatorClasses = cpg.typeDecl
      .filter(node =>
        node.inheritsFromTypeFullName.nonEmpty && node.inheritsFromTypeFullName.headOption
          .getOrElse("")
          .matches(".*airflow.*BaseOperator.*")
      )

    customOperatorClasses.foreach(node => {
      if (node.name.nonEmpty) {
        cpg.call
          .nameExact(node.name)
          .foreach(callNode => {
            // Tag the custom operator call
            addRuleTags(builder, callNode, ruleInfo, ruleCache)
          })
      }
    })
  }
}
