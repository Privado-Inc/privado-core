package ai.privado.model.exporter

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

//Need this to encode/decode DataFlowSubCategoryPathExcerptModel
import ai.privado.model.exporter.DataFlowEncoderDecoder._

case class ViolationProcessingModel(sourceId: String, occurrence: DataFlowSubCategoryPathExcerptModel)

case class ViolationDataFlowModel(sourceId: String, sinkId: String, pathIds: List[String])

case class ViolationPolicyDetailsModel(
  name: String,
  policyType: String,
  description: String,
  fix: String,
  action: String,
  tags: Map[String, String]
)

case class ViolationModel(
  policyId: String,
  policyDetails: Option[ViolationPolicyDetailsModel],
  dataFlow: Option[List[ViolationDataFlowModel]],
  processing: Option[List[ViolationProcessingModel]]
)

object ViolationEncoderDecoder {
  implicit val violationProcessingModelDecoder: Decoder[ViolationProcessingModel] =
    deriveDecoder[ViolationProcessingModel]
  implicit val violationProcessingModelEncoder: Encoder[ViolationProcessingModel] =
    deriveEncoder[ViolationProcessingModel]

  implicit val violationDataFlowModelDecoder: Decoder[ViolationDataFlowModel] = deriveDecoder[ViolationDataFlowModel]
  implicit val violationDataFlowModelEncoder: Encoder[ViolationDataFlowModel] = deriveEncoder[ViolationDataFlowModel]

  implicit val violationPolicyDetailsDecoder: Decoder[ViolationPolicyDetailsModel] =
    deriveDecoder[ViolationPolicyDetailsModel]
  implicit val violationPolicyDetailsEncoder: Encoder[ViolationPolicyDetailsModel] =
    deriveEncoder[ViolationPolicyDetailsModel]

  implicit val violationDecoder: Decoder[ViolationModel] = deriveDecoder[ViolationModel]
  implicit val violationEncoder: Encoder[ViolationModel] = deriveEncoder[ViolationModel]
}
