package ai.privado.exporter

import ai.privado.model.Constants
import ai.privado.model.exporter.SourceProcessingModel
import ai.privado.model.exporter.SourceEncoderDecoder.*
import io.circe.Json

import scala.collection.mutable

trait SourceExporterValidator {

  def getProcessings(outputMap: Map[String, Json]): List[SourceProcessingModel] = {
    val processings = outputMap(Constants.processing)
      .as[List[SourceProcessingModel]]
      .getOrElse(List())

    processings
  }
}
