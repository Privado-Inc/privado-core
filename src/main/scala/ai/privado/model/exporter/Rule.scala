package ai.privado.model.exporter

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

trait RuleInfoTrait {
  val id: String
  val name: String
  val category: String
  val sensitivity: String
  val isSensitive: Boolean
  val tags: Map[String, String]
}

trait RuleInfoWithDomainTrait extends RuleInfoTrait {
  val domains: Array[String]
}

case class RuleInfo(
  override val id: String,
  override val name: String,
  override val category: String,
  override val domains: Array[String],
  override val sensitivity: String,
  override val isSensitive: Boolean,
  override val tags: Map[String, String]
) extends RuleInfoWithDomainTrait

object RuleEncoderDecoder {
  implicit val ruleInfoDecoder: Decoder[RuleInfo] = deriveDecoder[RuleInfo]
  implicit val ruleInfoEncoder: Encoder[RuleInfo] = deriveEncoder[RuleInfo]
}
