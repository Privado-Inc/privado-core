package ai.privado.model.exporter

import io.circe.{Decoder, Encoder, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class DataFlowSubCategoryModel(sourceId: String, sinks: List[DataFlowSubCategorySinkModel])

case class DataFlowSubCategorySinkModel(
  sinkType: String,
  override val id: String,
  override val name: String,
  override val category: String,
  override val domains: Array[String],
  override val sensitivity: String,
  override val isSensitive: Boolean,
  override val tags: Map[String, String],
  apiUrl: List[String],
  paths: Json
) extends RuleInfoTrait

object DataFlowEncoderDecoder {
  implicit val dataFlowSubCategoryModelDecoder: Decoder[DataFlowSubCategoryModel] =
    deriveDecoder[DataFlowSubCategoryModel]
  implicit val dataFlowSubCategoryModelEncoder: Encoder[DataFlowSubCategoryModel] =
    deriveEncoder[DataFlowSubCategoryModel]

  implicit val dataFlowSubCategorySinkModelDecoder: Decoder[DataFlowSubCategorySinkModel] =
    deriveDecoder[DataFlowSubCategorySinkModel]
  implicit val dataFlowSubCategorySinkModelEncoder: Encoder[DataFlowSubCategorySinkModel] =
    deriveEncoder[DataFlowSubCategorySinkModel]
}
