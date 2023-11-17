package ai.privado.tagger.source

import ai.privado.cache.RuleCache
import ai.privado.feeder.MiniatureRuleModel
import ai.privado.model.{CatLevelOne, InternalTag, Language, NodeType, RuleInfo}
import ai.privado.semantic.Language.*
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class AndroidXmlPermissionTagger(cpg: Cpg, ruleCache: RuleCache, permissionRules: List[MiniatureRuleModel])
    extends PrivadoParallelCpgPass[MiniatureRuleModel](cpg) {

  override def generateParts(): Array[MiniatureRuleModel] = {

    // Create and add extra rules which are not present in Data element rules
    extraRules.foreach(ruleCache.setRuleInfo)
    permissionRules.toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, permissionRule: MiniatureRuleModel): Unit = {

    ruleCache.getRuleInfo(permissionRule.id) match
      case Some(ruleInfo) =>
        cpg.androidXmlPermissionNode
          .permissionType(permissionRule.pattern)
          .foreach(permissionNode => {
            storeForTag(builder, permissionNode, ruleCache)(InternalTag.VARIABLE_REGEX_IDENTIFIER.toString)
            addRuleTags(builder, permissionNode, ruleInfo, ruleCache)
          })
      case None =>
  }

  private val extraRules = List(
    RuleInfo(
      "Data.Sensitive.LocationData.ApproximateLocation",
      "Approximate Location",
      "Location Data",
      Array(),
      List(""),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    )
  )

}
