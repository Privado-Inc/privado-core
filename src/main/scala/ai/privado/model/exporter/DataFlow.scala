package ai.privado.model.exporter

import ai.privado.model.{DatabaseDetails, DatabaseSchema, DatabaseTable, DatabaseColumn}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import ai.privado.model.DatabaseEncoderDecoder._ //Required for DatabaseDetails Model

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
  databaseDetails: DatabaseDetails,
  paths: List[DataFlowSubCategoryPathModel]
) extends RuleInfoWithDomainTrait

trait DataFlowSubCategoryPathExcerptTrait {
  val sample: String
  val lineNumber: Int
  val columnNumber: Int
  val fileName: String
  val excerpt: String
}

case class DataFlowSubCategoryPathExcerptModel(
  override val sample: String,
  override val lineNumber: Int,
  override val columnNumber: Int,
  override val fileName: String,
  override val excerpt: String,
  val arguments: Option[List[(String, String)]] = None
) extends DataFlowSubCategoryPathExcerptTrait

case class DataFlowSubCategoryPathModel(pathId: String, path: List[DataFlowSubCategoryPathExcerptModel])

case class DataFlowSourceIntermediateModel(sourceId: String, sinks: List[DataFlowSinkIntermediateModel])

case class DataFlowSinkIntermediateModel(id: String, paths: List[DataFlowSubCategoryPathIntermediateModel])

case class DataFlowSubCategoryPathIntermediateModel(
  pathId: String,
  path: List[Option[DataFlowSubCategoryPathExcerptModel]]
)

case class DataFlowPathIntermediateModel(
  sourceId: String,
  sinkId: String,
  pathId: String,
  paths: List[Option[DataFlowSubCategoryPathExcerptModel]]
)

object DataFlowEncoderDecoder {
  implicit val dataFlowSubCategoryModelDecoder: Decoder[DataFlowSubCategoryModel] =
    deriveDecoder[DataFlowSubCategoryModel]
  implicit val dataFlowSubCategoryModelEncoder: Encoder[DataFlowSubCategoryModel] =
    deriveEncoder[DataFlowSubCategoryModel]

  implicit val dataFlowSubCategorySinkModelDecoder: Decoder[DataFlowSubCategorySinkModel] =
    deriveDecoder[DataFlowSubCategorySinkModel]
  implicit val dataFlowSubCategorySinkModelEncoder: Encoder[DataFlowSubCategorySinkModel] =
    deriveEncoder[DataFlowSubCategorySinkModel]

  implicit val dataFlowSubCategoryPathExcerptModelDecoder: Decoder[DataFlowSubCategoryPathExcerptModel] =
    deriveDecoder[DataFlowSubCategoryPathExcerptModel]
  implicit val dataFlowSubCategoryPathExcerptModelEncoder: Encoder[DataFlowSubCategoryPathExcerptModel] =
    deriveEncoder[DataFlowSubCategoryPathExcerptModel]

  implicit val dataFlowSubCategoryPathModelDecoder: Decoder[DataFlowSubCategoryPathModel] =
    deriveDecoder[DataFlowSubCategoryPathModel]
  implicit val dataFlowSubCategoryPathModelEncoder: Encoder[DataFlowSubCategoryPathModel] =
    deriveEncoder[DataFlowSubCategoryPathModel]

  implicit val dataFlowPathModelIntermediateDecoder: Decoder[DataFlowPathIntermediateModel] =
    deriveDecoder[DataFlowPathIntermediateModel]
  implicit val dataFlowPathModelIntermediateEncoder: Encoder[DataFlowPathIntermediateModel] =
    deriveEncoder[DataFlowPathIntermediateModel]

  implicit val dataFlowSubCategoryPathIntermediateModelDecoder: Decoder[DataFlowSubCategoryPathIntermediateModel] =
    deriveDecoder[DataFlowSubCategoryPathIntermediateModel]
  implicit val dataFlowSubCategoryPathIntermediateModelEncoder: Encoder[DataFlowSubCategoryPathIntermediateModel] =
    deriveEncoder[DataFlowSubCategoryPathIntermediateModel]

  implicit val DataFlowSourceIntermediateModelDecoder: Decoder[DataFlowSourceIntermediateModel] =
    deriveDecoder[DataFlowSourceIntermediateModel]
  implicit val DataFlowSourceIntermediateModelEncoder: Encoder[DataFlowSourceIntermediateModel] =
    deriveEncoder[DataFlowSourceIntermediateModel]

  implicit val DataFlowSinkIntermediateModelDecoder: Decoder[DataFlowSinkIntermediateModel] =
    deriveDecoder[DataFlowSinkIntermediateModel]
  implicit val DataFlowSinkIntermediateModelEncoder: Encoder[DataFlowSinkIntermediateModel] =
    deriveEncoder[DataFlowSinkIntermediateModel]

}
