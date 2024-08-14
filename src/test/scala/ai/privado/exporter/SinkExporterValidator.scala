package ai.privado.exporter

import ai.privado.model.Constants
import ai.privado.model.exporter.SinkProcessingModel
import ai.privado.model.exporter.SinkEncoderDecoder.*
import ai.privado.model.exporter.SinkModel
import io.circe.Json

trait SinkExporterValidator {

  def getSinkProcessings(outputMap: Map[String, Json]): List[SinkProcessingModel] = {
    val processings = outputMap(Constants.sinkProcessing)
      .as[List[SinkProcessingModel]]
      .getOrElse(List())
    processings
  }

  def getSinks(outputMap: Map[String, Json]): List[SinkModel] = {
    val sinks = outputMap(Constants.sinks)
      .as[List[SinkModel]]
      .getOrElse(List())
    sinks
  }
}
