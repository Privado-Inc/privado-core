package ai.privado.languageEngine.java.tagger.sink.api

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.sink.api.Utility.tagAPICallByItsUrlMethod
import ai.privado.model.{Constants, NodeType}
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Literal, Method}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory.getLogger

import scala.util.{Failure, Success, Try}

class JavaAPIRetrofitTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[(Call, Call)](cpg) {

  private val apiMatchingRegex =
    ruleCache.getAllRuleInfo.filter(_.nodeType == NodeType.API).map(_.combinedRulePattern).mkString("(", "|", ")")

  private val thirdPartyRuleInfo = ruleCache.getRuleInfo(Constants.thirdPartiesAPIRuleId)

  private val logger = getLogger(this.getClass)
  override def generateParts(): Array[(Call, Call)] = {
    cpg
      .call("create")
      .code(".*class.*")
      .map(c => (c, c.start.repeat(_.receiver.isCall)(_.until(_.name("baseUrl"))).l))
      .filter(_._2.nonEmpty)
      .map(item => (item._1, item._2.head))
      .toArray

  }

  override def runOnPart(builder: DiffGraphBuilder, createCallWithBaseUrlCall: (Call, Call)): Unit = {

    val createCall  = createCallWithBaseUrlCall._1
    val baseUrlCall = createCallWithBaseUrlCall._2

    // Strip .class and ::.class.java as after stripping these we get the class name of client
    val clientClassName = Try(
      createCall.argument.isCall.name(Operators.fieldAccess).head.code.stripSuffix(".class").stripSuffix("::class.java")
    ).toOption

    if (clientClassName.isDefined) {
      val sinkCalls = cpg.call.methodFullName(s".*${clientClassName.get}[.].*").nameNot("getClass").l

      Try(baseUrlCall.argument.last).toOption match
        case Some(lit: Literal) =>
          // Mark the nodes as API sink
          tagAPICallByItsUrlMethod(cpg, builder, lit, sinkCalls, apiMatchingRegex, thirdPartyRuleInfo, ruleCache)
        case _ =>
          Try {
            val methodNode = createCall.method
            tagAPICallByItsUrlMethod(
              cpg,
              builder,
              methodNode,
              sinkCalls,
              apiMatchingRegex,
              thirdPartyRuleInfo,
              ruleCache
            )
          } match
            case Failure(e) => logger.debug(s"Failed to get to a method node for retrofit create call")
            case _          =>
    }
  }
}
