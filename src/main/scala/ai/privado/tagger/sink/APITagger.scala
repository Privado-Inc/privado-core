package ai.privado.tagger.sink

import ai.privado.model.{InternalTags, RuleInfo}
import ai.privado.utility.Utilities
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.SimpleCpgPass
import overflowdb.BatchedUpdate
import io.shiftleft.semanticcpg.language._
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.EngineContext
import io.joern.dataflowengineoss.semanticsloader.{Parser, Semantics}
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.utils.ProjectRoot

import java.nio.file.Paths

class APITagger(cpg: Cpg, ruleInfo: RuleInfo) extends SimpleCpgPass(cpg) {

  lazy val APISINKS_REGEX =
    "(?i).*(?:url|client|connection|request|execute|load|host|access|fetch|get|set|put|post|trace|patch|send|remove|delete|write|read|assignment|provider).*"

  override def run(builder: BatchedUpdate.DiffGraphBuilder): Unit = {
    val apiInternalSinkPattern = cpg.literal.code(ruleInfo.pattern).l
    val apis                   = cpg.call.methodFullName(APISINKS_REGEX).l

    implicit val engineContext: EngineContext = EngineContext(Utilities.getDefaultSemantics())
    val apiFlows                              = apis.reachableByFlows(apiInternalSinkPattern).l

    apiFlows.foreach(flow => {
      val literalCode = flow.elements.head.code
      val apiNode     = flow.elements.last
      if (apiNode.tag.name(ruleInfo.nodeType).l.isEmpty) // Check so that we are not tagging same node again as sink
        addRuleTags(builder, apiNode, ruleInfo)
      storeForTag(builder, apiNode)(InternalTags.API_URL.toString, literalCode)
    })
  }
}
