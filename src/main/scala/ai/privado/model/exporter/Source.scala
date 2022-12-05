package ai.privado.model.exporter

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import ai.privado.model.exporter.DataFlowEncoderDecoder._ //Required for DataFlowSubCategoryPathExcerptModel

case class SourceModel(
  sourceType: String,
  override val id: String,
  override val name: String,
  override val category: String,
  override val sensitivity: String,
  override val isSensitive: Boolean,
  override val tags: Map[String, String]
) extends RuleInfoTrait

case class SourceProcessingModel(sourceId: String, occurrences: List[DataFlowSubCategoryPathExcerptModel])

object SourceEncoderDecoder {

  implicit val sourceModelDecoder: Decoder[SourceModel] = deriveDecoder[SourceModel]
  implicit val sourceModelEncoder: Encoder[SourceModel] = deriveEncoder[SourceModel]

  implicit val sourceProcessingModelDecoder: Decoder[SourceProcessingModel] = deriveDecoder[SourceProcessingModel]
  implicit val sourceProcessingModelEncoder: Encoder[SourceProcessingModel] = deriveEncoder[SourceProcessingModel]

}
