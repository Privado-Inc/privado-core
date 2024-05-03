package ai.privado.languageEngine.java.tagger.sink.framework.flink

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.storeForTag
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.semanticcpg.language.*

/** User can define there own flink sink connectors, this tagger focuses on identifying such class and tagging the
  * appropriate sink
  */
class FlinkUserDefinedConnectorTagger(cpg: Cpg, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[TypeDecl](cpg)
    with TaggerHelper {

  // Not using the exact package info, as for kotlin that info is missing
  private val inheritsFromRegex = ".*(Sink|SinkFunction|RichSinkFunction)(<.*>)?"

  // TODO, Need to check if the retrieved class is a abstract or interface, in which case, needs to again fetch the new one
  override def generateParts(): Array[TypeDecl] =
    cpg.typeDecl.filter(_.inheritsFromTypeFullName.exists(_.matches(inheritsFromRegex))).toArray

  override def runOnPart(builder: DiffGraphBuilder, typeDeclNode: TypeDecl): Unit = {

    // Fetch all the sink nodes present in the typeDecl, except leakages
    val sinkNodesInTypeDecl = typeDeclNode.ast
      .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
      .whereNot(_.tag.nameExact(Constants.catLevelTwo).value(Constants.leakages))
      .distinctBy(_.tag.nameExact(Constants.id).value.headOption.getOrElse(""))
      .l

    /* Fetch all the local, which are basically the creation of these custom sinks, partition them into
       localAndSinkInSameMethod - custom sink creation and flink sink call, present in the same enclosed method
       localWithoutSink - flink sink call is not present in the enclosed method having initialisation of custom sink
     */
    val (localAndSinkInSameMethod, localWithoutSink) =
      cpg.local
        .typeFullName(s"${typeDeclNode.fullName}(<.*>)?")
        .whereNot(_.nameExact("this"))
        .l
        .partition(_.method.ast.isCall.name(flinkSinkName).nonEmpty)

    localAndSinkInSameMethod.method.ast.isCall.name(flinkSinkName).dedup.foreach { flinkSink =>
      copyTags(builder, ruleCache, sinkNodesInTypeDecl, flinkSink)
    }

    if (localAndSinkInSameMethod.isEmpty && localWithoutSink.nonEmpty) {

      // Not Running Dataflow, as this can cause performance issues, Instead copying tags to the initialisation local nodes,
      // and these local nodes will be used run dataflow in a separate pass to use the dataflow parallelism to fullest
      localWithoutSink.dedup.foreach { sinkLocalNode =>
        copyTags(builder, ruleCache, sinkNodesInTypeDecl, sinkLocalNode)
        storeForTag(builder, sinkLocalNode, ruleCache)(InternalTag.FLINK_INITIALISATION_LOCAL_NODE.toString)
      }
    }
  }
}
