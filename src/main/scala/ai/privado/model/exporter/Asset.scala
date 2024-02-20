package ai.privado.model.exporter

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

case class PropertyNodesModel(key: String, value: String, fileName: String)

object PropertyNodesEncoderDecoder {

  implicit val propertyNodesModelDecoder: Decoder[PropertyNodesModel] = deriveDecoder[PropertyNodesModel]
  implicit val propertyNodesModelEncoder: Encoder[PropertyNodesModel] = deriveEncoder[PropertyNodesModel]
}
