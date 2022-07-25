package ai.privado.model

import io.circe.Decoder.Result
import io.circe.{Decoder, HCursor}

import scala.collection.immutable.HashMap

case class RuleInfo(
  id: String,
  name: String,
  category: String,
  patterns: List[String],
  sensitivity: String,
  tags: Map[String, String],
  nodeType: String
)
case class Rules(sources: List[RuleInfo], sinks: List[RuleInfo])

object CirceEnDe {

  implicit val decodeRules: Decoder[Rules] = new Decoder[Rules] {
    override def apply(c: HCursor): Result[Rules] = {
      val sources = c.downField("sources").as[List[RuleInfo]]
      val sinks   = c.downField("sinks").as[List[RuleInfo]]
      Right(Rules(sources = sources.getOrElse(List[RuleInfo]()), sinks = sinks.getOrElse(List[RuleInfo]())))
    }
  }
  implicit val decodeRuleInfo: Decoder[RuleInfo] = new Decoder[RuleInfo] {
    override def apply(c: HCursor): Result[RuleInfo] = {
      val id          = c.downField(Constants.id).as[String]
      val name        = c.downField(Constants.name).as[String]
      val category    = c.downField(Constants.category).as[String]
      val patterns    = c.downField(Constants.patterns).as[List[String]]
      val sensitivity = c.downField(Constants.sensitivity).as[String]
      val tags        = c.downField(Constants.tags).as[Map[String, String]]
      Right(
        RuleInfo(
          id.getOrElse(""),
          name.getOrElse(""),
          category.getOrElse(""),
          patterns = patterns.getOrElse(List[String]()),
          sensitivity = sensitivity.getOrElse(""),
          tags = tags.getOrElse(HashMap[String, String]()),
          nodeType = ""
        )
      )
    }
  }
}
