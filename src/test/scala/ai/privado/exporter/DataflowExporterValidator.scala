package ai.privado.exporter

import ai.privado.model.Constants
import ai.privado.model.exporter.DataFlowSubCategoryModel
import ai.privado.model.exporter.DataFlowEncoderDecoder.*
import ai.privado.model.exporter.DataFlowSubCategoryModel
import io.circe.Json

import scala.collection.mutable

trait DataflowExporterValidator {

  def getLeakageFlows(outputMap: Map[String, Json]): List[DataFlowSubCategoryModel] = {
    val alldataflows = outputMap(Constants.dataFlow)
      .as[mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]]]
      .getOrElse(Map.empty[String, List[DataFlowSubCategoryModel]])

    alldataflows(Constants.leakages)
  }

  def getStorageFlows(outputMap: Map[String, Json]): List[DataFlowSubCategoryModel] = {
    val allDataflows = outputMap(Constants.dataFlow)
      .as[mutable.LinkedHashMap[String, List[DataFlowSubCategoryModel]]]
      .getOrElse(Map.empty[String, List[DataFlowSubCategoryModel]])

    allDataflows(Constants.storages)
  }
}
