package ai.privado.model

import io.circe.Decoder.Result
import io.circe.{Decoder, HCursor}

import scala.collection.immutable.HashMap

case class RuleInfo(
  id: String,
  name: String,
  category: String,
  domains: Array[String],
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
case class ConfigAndRules(
  sources: List[RuleInfo],
  sinks: List[RuleInfo],
  collections: List[RuleInfo],
  policies: List[Policy],
  exclusions: List[RuleInfo]
)

case class DataFlow(sources: List[String], sinks: List[String])

case class Policy(
  id: String,
  description: String,
  action: PolicyAction.PolicyAction,
  dataFlow: DataFlow,
  repositories: List[String],
  tags: Map[String, String],
  file: String,
  categoryTree: Array[String]
)

object CirceEnDe {

  implicit val decodePolicy: Decoder[Policy] = new Decoder[Policy] {
    override def apply(c: HCursor): Result[Policy] = {
      val id           = c.downField(Constants.id).as[String]
      val description  = c.downField(Constants.description).as[String]
      val action       = c.downField(Constants.action).as[String]
      val dataFlow     = c.downField(Constants.dataFlow).as[DataFlow]
      val repositories = c.downField(Constants.repositories).as[List[String]]
      val tags         = c.downField(Constants.tags).as[Map[String, String]]
      Right(
        Policy(
          id = id.getOrElse(""),
          description = description.getOrElse(""),
          action = PolicyAction.withNameDefaultHandler(action.getOrElse("")),
          dataFlow = dataFlow.getOrElse(DataFlow(List[String](), List[String]())),
          repositories = repositories.getOrElse(List[String]()),
          tags = tags.getOrElse(HashMap[String, String]()),
          file = "",
          categoryTree = Array[String]()
        )
      )
    }
  }

  implicit val decodeDataFlow: Decoder[DataFlow] = new Decoder[DataFlow] {
    override def apply(c: HCursor): Result[DataFlow] = {
      val sources = c.downField(Constants.sources).as[List[String]]
      val sinks   = c.downField(Constants.sinks).as[List[String]]
      Right(DataFlow(sources = sources.getOrElse(List[String]()), sinks = sinks.getOrElse(List[String]())))
    }
  }

  implicit val decodeRules: Decoder[ConfigAndRules] = new Decoder[ConfigAndRules] {
    override def apply(c: HCursor): Result[ConfigAndRules] = {
      val sources     = c.downField(Constants.sources).as[List[RuleInfo]]
      val sinks       = c.downField(Constants.sinks).as[List[RuleInfo]]
      val collections = c.downField(Constants.collections).as[List[RuleInfo]]
      val policies    = c.downField(Constants.policies).as[List[Policy]]
      val exclusions  = c.downField(Constants.exclusions).as[List[RuleInfo]]
      Right(
        ConfigAndRules(
          sources = sources.getOrElse(List[RuleInfo]()),
          sinks = sinks.getOrElse(List[RuleInfo]()),
          collections = collections.getOrElse(List[RuleInfo]()),
          policies = policies.getOrElse(List[Policy]()),
          exclusions = exclusions.getOrElse(List[RuleInfo]())
        )
      )
    }
  }
  implicit val decodeRuleInfo: Decoder[RuleInfo] = new Decoder[RuleInfo] {
    override def apply(c: HCursor): Result[RuleInfo] = {
      val id          = c.downField(Constants.id).as[String]
      val name        = c.downField(Constants.name).as[String]
      val category    = c.downField(Constants.category).as[String]
      val domains     = c.downField(Constants.domains).as[Array[String]]
      val patterns    = c.downField(Constants.patterns).as[List[String]]
      val isSensitive = c.downField(Constants.isSensitive).as[Boolean]
      val sensitivity = c.downField(Constants.sensitivity).as[String]
      val tags        = c.downField(Constants.tags).as[Map[String, String]]
      Right(
        RuleInfo(
          id = id.getOrElse(""),
          name = name.getOrElse(""),
          category = category.getOrElse(""),
          domains = domains.getOrElse(Array[String]()),
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
