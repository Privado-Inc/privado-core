package ai.privado.model

import io.circe.Decoder.Result
import io.circe.{Decoder, HCursor}

import scala.collection.immutable.HashMap

case class RuleInfo(
  id: String,
  name: String,
  category: String,
  patterns: List[String],
  isSensitive: Boolean,
  sensitivity: String,
  tags: Map[String, String],
  nodeType: NodeType.NodeType,
  file: String,
  catLevelOne: CatLevelOne.CatLevelOne,
  catLevelTwo: String,
  language: Language.Language,
  categoryTree: Array[String]
)
case class Rules(sources: List[RuleInfo], sinks: List[RuleInfo], collections: List[RuleInfo])

object CirceEnDe {

  implicit val decodeRules: Decoder[Rules] = new Decoder[Rules] {
    override def apply(c: HCursor): Result[Rules] = {
      val sources     = c.downField("sources").as[List[RuleInfo]]
      val sinks       = c.downField("sinks").as[List[RuleInfo]]
      val collections = c.downField("collections").as[List[RuleInfo]]
      Right(
        Rules(
          sources = sources.getOrElse(List[RuleInfo]()),
          sinks = sinks.getOrElse(List[RuleInfo]()),
          collections = collections.getOrElse(List[RuleInfo]())
        )
      )
    }
  }
  implicit val decodeRuleInfo: Decoder[RuleInfo] = new Decoder[RuleInfo] {
    override def apply(c: HCursor): Result[RuleInfo] = {
      val id          = c.downField(Constants.id).as[String]
      val name        = c.downField(Constants.name).as[String]
      val category    = c.downField(Constants.category).as[String]
      val patterns    = c.downField(Constants.patterns).as[List[String]]
      val isSensitive = c.downField(Constants.isSensitive).as[Boolean]
      val sensitivity = c.downField(Constants.sensitivity).as[String]
      val tags        = c.downField(Constants.tags).as[Map[String, String]]
      Right(
        RuleInfo(
          id = id.getOrElse(""),
          name = name.getOrElse(""),
          category = category.getOrElse(""),
          patterns = patterns.getOrElse(List[String]()),
          sensitivity = sensitivity.getOrElse(""),
          isSensitive = isSensitive.getOrElse(false),
          tags = tags.getOrElse(HashMap[String, String]()),
          nodeType = NodeType.REGULAR,
          file = "",
          catLevelOne = CatLevelOne.UNKNOWN,
          catLevelTwo = "",
          language = Language.UNKNOWN,
          categoryTree = Array[String]()
        )
      )
    }
  }
}
