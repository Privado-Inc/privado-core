package ai.privado.model.exporter

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class CollectionOccurrenceModel(
  endPoint: String,
  override val sample: String,
  override val lineNumber: Int,
  override val columnNumber: Int,
  override val fileName: String,
  override val excerpt: String
) extends DataFlowSubCategoryPathExcerptTrait

case class CollectionOccurrenceDetailModel(sourceId: String, occurrences: List[CollectionOccurrenceModel])

case class CollectionModel(
  collectionId: String,
  name: String,
  isSensitive: Boolean,
  collections: List[CollectionOccurrenceDetailModel]
)

object CollectionEncoderDecoder {

  implicit val collectionOccurrenceModelDecoder: Decoder[CollectionOccurrenceModel] =
    deriveDecoder[CollectionOccurrenceModel]
  implicit val collectionOccurrenceModelEncoder: Encoder[CollectionOccurrenceModel] =
    deriveEncoder[CollectionOccurrenceModel]

  implicit val collectionOccurrenceDetailModelDecoder: Decoder[CollectionOccurrenceDetailModel] =
    deriveDecoder[CollectionOccurrenceDetailModel]
  implicit val collectionOccurrenceDetailModelEncoder: Encoder[CollectionOccurrenceDetailModel] =
    deriveEncoder[CollectionOccurrenceDetailModel]

  implicit val collectionModelDecoder: Decoder[CollectionModel] = deriveDecoder[CollectionModel]
  implicit val collectionModelEncoder: Encoder[CollectionModel] = deriveEncoder[CollectionModel]
}
