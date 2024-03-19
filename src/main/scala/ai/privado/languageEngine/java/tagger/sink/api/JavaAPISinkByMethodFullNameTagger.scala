package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, InternalTag}
import ai.privado.tagger.{PrivadoParallelCpgPass, PrivadoSimpleCpgPass}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}

import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.language.postfixOps

class JavaAPISinkByMethodFullNameTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoSimpleCpgPass(cpg) {

  private val apiMethodFullNameRegex = ruleCache.getSystemConfigByKey(Constants.apiMethodFullNames)

  val cacheCall: List[Call] = cpg.call.or(_.nameNot(Operators.ALL.asScala.toSeq.appended("init"): _*)).l
  override def run(builder: DiffGraphBuilder): Unit = {
    if (apiMethodFullNameRegex.nonEmpty) {
      val sinkCalls = cacheCall.methodFullName(apiMethodFullNameRegex).toArray

      // Mark the nodes as API sink
      sinkCalls.foreach(storeForTag(builder, _, ruleCache)(InternalTag.API_SINK_MARKED.toString))
    }
  }

}
