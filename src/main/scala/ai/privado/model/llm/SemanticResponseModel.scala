package ai.privado.model.llm

import ai.privado.model.exporter.DataFlowSubCategoryPathExcerptModel
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}


case class ArgumentParamModel(_1: Option[Int], _2: Option[String], _3: String)

case class ScoreModel(label: String, score: Double)
case class ArgumentModel(param1: ArgumentParamModel, param2: ArgumentParamModel, result: ScoreModel)

case class SemanticCallModel(id: Long, code: String, returnValue: String, arguments: List[ArgumentModel])
case class SemanticResponseModel (message: String, output: List[SemanticCallModel])

object SemanticResponseEncoderDecoder {

  implicit val argumentParamModelDecoder: Decoder[ArgumentParamModel] =
    deriveDecoder[ArgumentParamModel]
  implicit val argumentParamModelEncoder: Encoder[ArgumentParamModel] =
    deriveEncoder[ArgumentParamModel]

  implicit val scoreModelDecoder: Decoder[ScoreModel] =
    deriveDecoder[ScoreModel]
  implicit val scoreModelEncoder: Encoder[ScoreModel] =
    deriveEncoder[ScoreModel]

  implicit val argumentModelDecoder: Decoder[ArgumentModel] =
    deriveDecoder[ArgumentModel]
  implicit val argumentModelEncoder: Encoder[ArgumentModel] =
    deriveEncoder[ArgumentModel]

  implicit val semanticCallModelDecoder: Decoder[SemanticCallModel] =
    deriveDecoder[SemanticCallModel]
  implicit val semanticCallModelEncoder: Encoder[SemanticCallModel] =
    deriveEncoder[SemanticCallModel]

  implicit val semanticResponseModelDecoder: Decoder[SemanticResponseModel] =
    deriveDecoder[SemanticResponseModel]
  implicit val semanticResponseModelEncoder: Encoder[SemanticResponseModel] =
    deriveEncoder[SemanticResponseModel]
}
