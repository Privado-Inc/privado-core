package ai.privado.exporter

import ai.privado.model.Constants
import ai.privado.model.exporter.{
  DataFlowSubCategoryModel,
  DataFlowSubCategoryPathExcerptModel,
  DataFlowSubCategoryPathModel,
  DataFlowSubCategorySinkModel
}
import io.circe.Json
import io.joern.dataflowengineoss.language.Path
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import ai.privado.model.exporter.DataFlowEncoderDecoder.*

import scala.collection.mutable
import scala.util.Try

trait DataflowExporterValidator extends Matchers {

  def getLeakageFlows(outputMap: Map[String, Json]): List[DataFlowSubCategoryModel] = {
    val alldataflows = outputMap(Constants.dataFlow)
      .as[mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]]]
      .getOrElse(Map.empty[String, List[DataFlowSubCategoryModel]])

    alldataflows(Constants.leakages)
  }

  def validateLineNumberForDataflowStep(
    step: DataFlowSubCategoryPathExcerptModel,
    expectedLineNumber: Int
  ): Assertion = {
    step.lineNumber shouldBe expectedLineNumber
  }

  def getHeadStepOfDataflow(
    dataflow: DataFlowSubCategoryModel,
    sinkId: String
  ): Option[DataFlowSubCategoryPathExcerptModel] = {
    val dataflowForSink = dataflow.sinks.find(sink => sink.id.equals(sinkId))
    Try(
      dataflowForSink.get.paths.headOption
        .getOrElse(DataFlowSubCategoryPathModel("", List.empty[DataFlowSubCategoryPathExcerptModel]))
        .path
        .head
    ).toOption
  }

  def getDataflowForSourceId(
    sourceId: String,
    dataflows: List[DataFlowSubCategoryModel]
  ): Option[DataFlowSubCategoryModel] = {
    dataflows
      .find(flow => flow.sourceId.equals(sourceId))
  }

  def getStorageFlows(outputMap: Map[String, Json]): List[DataFlowSubCategoryModel] = {
    val allDataflows = outputMap(Constants.dataFlow)
      .as[mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]]]
      .getOrElse(Map.empty[String, List[DataFlowSubCategoryModel]])

    allDataflows(Constants.storages)
  }
}
