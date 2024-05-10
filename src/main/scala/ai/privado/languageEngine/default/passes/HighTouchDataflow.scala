package ai.privado.languageEngine.default.passes

import ai.privado.cache.DataFlowCache
import ai.privado.model.{CatLevelOne, Constants, DataFlowPathModel, InternalTag, NodeType}
import io.joern.dataflowengineoss.language.Path
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.semantic.Language.*
import ai.privado.languageEngine.default.*
import io.shiftleft.semanticcpg.language.*

object HighTouchDataflow {

  def generateDataflowAndAddToDataflowCache(cpg: Cpg, dataFlowCache: DataFlowCache): Unit = {

    val sources = cpg.sqlColumn.where(_.tag.nameExact(InternalTag.VARIABLE_REGEX_LITERAL.toString)).l
    val sinks   = cpg.highTouchSink.where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name)).l

    sinks.foreach(sink => {
      val sourceFlowingToSink = sources.where(_.file.name(s".*${sink.correspondingModel}.*")).l
      sourceFlowingToSink.foreach(source => {
        val pathId = s"${source.id()}-${sink.id()}"
        val path   = Path(List(source, sink))
        // create dataflow between each source and sink
        val sourceRuleId = source.tag.nameExact(Constants.id).value.headOption.getOrElse("")
        val sinkRuleId   = sink.tag.nameExact(Constants.id).value.headOption.getOrElse("")
        val _dataflowModel = DataFlowPathModel(
          sourceRuleId,
          sinkRuleId,
          Constants.third_parties,
          NodeType.REGULAR.toString,
          pathId,
          applyDedup = false // Pass this flag to not run the dedup logic on the generated dataflow
        )
        synchronized {
          // We need to update the dataflowsMap and set new dataflow using setDataflow function
          dataFlowCache.dataflowsMapByType.put(pathId, path)
          dataFlowCache.setDataflow(_dataflowModel)
        }
      })
    })
  }

}
