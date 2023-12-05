package ai.privado.model.exporter

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class AndroidPermissionModel(
  permissionType: String,
  isUsed: Boolean,
  permissionDetail: AndroidPermissionDetailModel
)

case class AndroidPermissionDetailModel(sourceId: String, occurrence: DataFlowSubCategoryPathExcerptModel)

object AndroidPermissionsEncoderDecoder {

  implicit val androidPermissionModelDecoder: Decoder[AndroidPermissionModel] = deriveDecoder[AndroidPermissionModel]
  implicit val androidPermissionModelEncoder: Encoder[AndroidPermissionModel] = deriveEncoder[AndroidPermissionModel]

  implicit val androidPermissionDetailModelDecoder: Decoder[AndroidPermissionDetailModel] =
    deriveDecoder[AndroidPermissionDetailModel]
  implicit val androidPermissionDetailModelEncoder: Encoder[AndroidPermissionDetailModel] =
    deriveEncoder[AndroidPermissionDetailModel]

}
