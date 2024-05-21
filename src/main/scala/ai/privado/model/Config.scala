/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

package ai.privado.model

import io.circe.Decoder.Result
import io.circe.{Decoder, HCursor, Json}
import io.shiftleft.codepropertygraph.generated.Cpg

import scala.collection.immutable.HashMap

case class RuleInfo(
  id: String,
  name: String,
  category: String,
  filterProperty: FilterProperty.FilterProperty,
  domains: Array[String],
  patterns: List[String],
  isSensitive: Boolean = false,
  sensitivity: String = "",
  tags: Map[String, String] = Map(),
  nodeType: NodeType.NodeType = NodeType.UNKNOWN,
  file: String = "",
  catLevelOne: CatLevelOne.CatLevelOne = CatLevelOne.UNKNOWN,
  catLevelTwo: String = "",
  language: Language.Language = Language.UNKNOWN,
  categoryTree: Array[String] = Array(),
  isGenerated: Boolean = false // mark this true, if the rule is generated by privado-core
) {
  def combinedRulePattern: String = {
    patterns.mkString("(", "|", ")")
  }
}

object RuleInfo {
  def getEmptyRuleInfo: RuleInfo = {
    RuleInfo(
      "",
      "",
      "",
      FilterProperty.CODE,
      Array.empty[String],
      List.empty[String],
      false,
      "",
      Map.empty[String, String],
      NodeType.UNKNOWN,
      "",
      CatLevelOne.COLLECTIONS,
      catLevelTwo = Constants.annotations,
      Language.PHP,
      Array()
    )
  }
}

case class ConfigAndRules(
  sources: List[RuleInfo] = List(),
  sinks: List[RuleInfo] = List(),
  collections: List[RuleInfo] = List(),
  policies: List[PolicyOrThreat] = List(),
  threats: List[PolicyOrThreat] = List(),
  exclusions: List[RuleInfo] = List(),
  semantics: List[Semantic] = List(),
  sinkSkipList: List[RuleInfo] = List(),
  systemConfig: List[SystemConfig] = List(),
  auditConfig: List[RuleInfo] = List(),
  inferences: List[RuleInfo] = List(),
  dedRules: List[DEDRuleInfo] = List()
)

case class AllowedSourceFilters(sources: List[String])
case class AllowedSinkFilters(domains: List[String])
case class SourceFilter(
  isSensitive: Option[Boolean],
  sensitivity: String,
  name: String,
  allowedSourceFilters: AllowedSourceFilters
)

case class SinkFilter(domains: List[String], sinkType: String, name: String, allowedSinkFilters: AllowedSinkFilters)

case class CollectionFilter(collectionType: String, endPoint: String)

case class DataFlow(
  sources: List[String],
  sourceFilters: SourceFilter,
  sinks: List[String],
  sinkFilters: SinkFilter,
  collectionFilters: CollectionFilter
)

case class PolicyOrThreat(
  id: String,
  description: String,
  name: String,
  fix: String,
  policyOrThreatType: PolicyThreatType.PolicyThreatType,
  action: PolicyAction.PolicyAction,
  dataFlow: DataFlow,
  repositories: List[String],
  tags: Map[String, String],
  config: Map[String, String],
  file: String,
  categoryTree: Array[String]
)

case class DEDVariable(name: String, typeInSrc: String, lineNumber: Option[Int])

case class DEDClassificationData(id: String, variables: List[DEDVariable])

case class DEDRuleInfo(id: String, filePath: String, classificationData: List[DEDClassificationData])

case class Semantic(
  signature: String,
  flow: String,
  file: String,
  language: Language.Language,
  categoryTree: Array[String]
)

case class SystemConfig(
  key: String,
  value: String,
  language: Language.Language = Language.UNKNOWN,
  file: String = "",
  categoryTree: Array[String] = Array()
)

object CirceEnDe {

  implicit val decodePolicy: Decoder[PolicyOrThreat] = new Decoder[PolicyOrThreat] {
    override def apply(c: HCursor): Result[PolicyOrThreat] = {
      val id                 = c.downField(Constants.id).as[String]
      val name               = c.downField(Constants.name).as[String]
      val fix                = c.downField(Constants.fix).as[String]
      val policyOrThreatType = c.downField(Constants.policyOrThreatType).as[String]
      val description        = c.downField(Constants.description).as[String]
      val action             = c.downField(Constants.action).as[String]
      val dataFlow           = c.downField(Constants.dataFlow).as[DataFlow]
      val repositories       = c.downField(Constants.repositories).as[List[String]]
      val tags               = c.downField(Constants.tags).as[Map[String, String]]
      val config             = c.downField(Constants.config).as[Map[String, String]]
      Right(
        PolicyOrThreat(
          id = id.getOrElse(""),
          name = name.getOrElse(""),
          fix = fix.getOrElse(""),
          policyOrThreatType = PolicyThreatType.withNameDefaultHandler(policyOrThreatType.getOrElse("")),
          description = description.getOrElse(""),
          action = PolicyAction.withNameDefaultHandler(action.getOrElse("")),
          dataFlow = dataFlow.getOrElse(
            DataFlow(
              List[String](),
              SourceFilter(None, "", "", AllowedSourceFilters(List[String]())),
              List[String](),
              SinkFilter(List[String](), "", "", AllowedSinkFilters(List[String]())),
              CollectionFilter("", "")
            )
          ),
          repositories = repositories.getOrElse(List[String]()),
          tags = tags.getOrElse(HashMap[String, String]()),
          config = config.getOrElse(HashMap[String, String]()),
          file = "",
          categoryTree = Array[String]()
        )
      )
    }
  }

  implicit val decodeAllowedSourceFilter: Decoder[AllowedSourceFilters] = new Decoder[AllowedSourceFilters] {
    override def apply(c: HCursor): Result[AllowedSourceFilters] = {
      Right(AllowedSourceFilters(sources = c.downField(Constants.sources).as[List[String]].getOrElse(List[String]())))
    }
  }

  implicit val decodeSourceFilter: Decoder[SourceFilter] = new Decoder[SourceFilter] {
    override def apply(c: HCursor): Result[SourceFilter] = {
      Right(
        SourceFilter(
          isSensitive = c.downField(Constants.isSensitive).as[Option[Boolean]].getOrElse(None),
          sensitivity = c.downField(Constants.sensitivity).as[String].getOrElse(""),
          name = c.downField(Constants.name).as[String].getOrElse(""),
          allowedSourceFilters = c
            .downField(Constants.allowedSourceFilters)
            .as[AllowedSourceFilters]
            .getOrElse(AllowedSourceFilters(List[String]()))
        )
      )
    }
  }

  implicit val decodeAllowedSinkFilter: Decoder[AllowedSinkFilters] = new Decoder[AllowedSinkFilters] {
    override def apply(c: HCursor): Result[AllowedSinkFilters] = {
      Right(AllowedSinkFilters(domains = c.downField(Constants.domains).as[List[String]].getOrElse(List[String]())))
    }
  }

  implicit val decodeSinkFilter: Decoder[SinkFilter] = new Decoder[SinkFilter] {
    override def apply(c: HCursor): Result[SinkFilter] = {
      Right(
        SinkFilter(
          domains = c.downField(Constants.domains).as[List[String]].getOrElse(List[String]()),
          sinkType = c.downField(Constants.sinkType).as[String].getOrElse(""),
          name = c.downField(Constants.name).as[String].getOrElse(""),
          allowedSinkFilters = c
            .downField(Constants.allowedSinkFilters)
            .as[AllowedSinkFilters]
            .getOrElse(AllowedSinkFilters(List[String]()))
        )
      )
    }
  }

  implicit val decodeCollectionFilter: Decoder[CollectionFilter] = new Decoder[CollectionFilter] {
    override def apply(c: HCursor): Result[CollectionFilter] = {
      Right(
        CollectionFilter(
          collectionType = c.downField(Constants.collectionType).as[String].getOrElse(""),
          endPoint = c.downField(Constants.endPoint).as[String].getOrElse("")
        )
      )
    }
  }

  implicit val decodeDataFlow: Decoder[DataFlow] = new Decoder[DataFlow] {
    override def apply(c: HCursor): Result[DataFlow] = {
      val sources          = c.downField(Constants.sources).as[List[String]]
      val sourceFilter     = c.downField(Constants.sourceFilters).as[SourceFilter]
      val sinks            = c.downField(Constants.sinks).as[List[String]]
      val sinkFilter       = c.downField(Constants.sinkFilters).as[SinkFilter]
      val collectionFilter = c.downField(Constants.collectionFilters).as[CollectionFilter]
      Right(
        DataFlow(
          sources = sources.getOrElse(List[String]()),
          sourceFilters = sourceFilter.getOrElse(SourceFilter(None, "", "", AllowedSourceFilters(List[String]()))),
          sinks = sinks.getOrElse(List[String]()),
          sinkFilters = sinkFilter.getOrElse(SinkFilter(List[String](), "", "", AllowedSinkFilters(List[String]()))),
          collectionFilters = collectionFilter.getOrElse(CollectionFilter("", ""))
        )
      )
    }
  }

  implicit val decodeSemantic: Decoder[Semantic] = new Decoder[Semantic] {
    override def apply(c: HCursor): Result[Semantic] = {
      val signature = c.downField(Constants.signature).as[String]
      val flow      = c.downField(Constants.flow).as[String]
      Right(
        Semantic(
          signature = signature.getOrElse(""),
          flow = flow.getOrElse(""),
          file = "",
          categoryTree = Array[String](),
          language = Language.UNKNOWN
        )
      )
    }
  }

  implicit val decodeSystemConfig: Decoder[SystemConfig] = new Decoder[SystemConfig] {
    override def apply(c: HCursor): Result[SystemConfig] = {
      val key   = c.downField(Constants.key).as[String]
      val value = c.downField(Constants.value).as[String]
      Right(
        SystemConfig(
          key = key.getOrElse(""),
          value = value.getOrElse(""),
          file = "",
          categoryTree = Array[String](),
          language = Language.UNKNOWN
        )
      )
    }
  }

  implicit val decodeDEDVariable: Decoder[DEDVariable] = new Decoder[DEDVariable] {
    override def apply(c: HCursor): Result[DEDVariable] = {
      val name       = c.downField(Constants.name).as[String]
      val typeInSrc  = c.downField(Constants.typeInSrc).as[String]
      val lineNumber = c.downField(Constants.lineNumber).as[Int]

      Right(
        DEDVariable(
          name = name.getOrElse(""),
          typeInSrc = typeInSrc.getOrElse(""),
          lineNumber = Some(lineNumber.getOrElse(Constants.defaultLineNumber))
        )
      )
    }
  }

  implicit val decodeDEDClassificationData: Decoder[DEDClassificationData] = new Decoder[DEDClassificationData] {
    override def apply(c: HCursor): Result[DEDClassificationData] = {
      val id        = c.downField(Constants.filePath).as[String]
      val variables = c.downField(Constants.variables).as[List[DEDVariable]]

      Right(DEDClassificationData(id = id.getOrElse(""), variables = variables.getOrElse(List[DEDVariable]())))
    }
  }

  implicit val decodeDEDRuleInfo: Decoder[DEDRuleInfo] = new Decoder[DEDRuleInfo] {
    override def apply(c: HCursor): Result[DEDRuleInfo] = {
      val id                 = c.downField(Constants.filePath).as[String]
      val filePath           = c.downField(Constants.filePath).as[String]
      val classificationData = c.downField(Constants.classificationData).as[List[DEDClassificationData]]

      Right(
        DEDRuleInfo(
          id = id.getOrElse(""),
          filePath = filePath.getOrElse(""),
          classificationData = classificationData.getOrElse(List[DEDClassificationData]())
        )
      )
    }
  }

  implicit val decodeRules: Decoder[ConfigAndRules] = new Decoder[ConfigAndRules] {
    override def apply(c: HCursor): Result[ConfigAndRules] = {
      val sources      = c.downField(Constants.sources).as[List[RuleInfo]]
      val sinks        = c.downField(Constants.sinks).as[List[RuleInfo]]
      val collections  = c.downField(Constants.collections).as[List[RuleInfo]]
      val policies     = c.downField(Constants.policies).as[List[PolicyOrThreat]]
      val exclusions   = c.downField(Constants.exclusions).as[List[RuleInfo]]
      val threats      = c.downField(Constants.threats).as[List[PolicyOrThreat]]
      val semantics    = c.downField(Constants.semantics).as[List[Semantic]]
      val sinkSkipList = c.downField(Constants.sinkSkipList).as[List[RuleInfo]]
      val systemConfig = c.downField(Constants.systemConfig).as[List[SystemConfig]]
      val auditConfig  = c.downField(Constants.auditConfig).as[List[RuleInfo]]
      val inferences   = c.downField(Constants.inferences).as[List[RuleInfo]]
      val dedRules     = c.downField(Constants.DED).as[List[DEDRuleInfo]]
      Right(
        ConfigAndRules(
          sources = sources.getOrElse(List[RuleInfo]()),
          sinks = sinks.getOrElse(List[RuleInfo]()),
          collections = collections.getOrElse(List[RuleInfo]()),
          policies = policies.getOrElse(List[PolicyOrThreat]()),
          exclusions = exclusions.getOrElse(List[RuleInfo]()),
          threats = threats.getOrElse(List[PolicyOrThreat]()),
          semantics = semantics.getOrElse(List[Semantic]()),
          sinkSkipList = sinkSkipList.getOrElse(List[RuleInfo]()),
          systemConfig = systemConfig.getOrElse(List[SystemConfig]()),
          auditConfig = auditConfig.getOrElse(List[RuleInfo]()),
          inferences = inferences.getOrElse(List[RuleInfo]()),
          dedRules = dedRules.getOrElse(List[DEDRuleInfo]())
        )
      )
    }
  }
  implicit val decodeRuleInfo: Decoder[RuleInfo] = new Decoder[RuleInfo] {
    override def apply(c: HCursor): Result[RuleInfo] = {
      val id             = c.downField(Constants.id).as[String]
      val name           = c.downField(Constants.name).as[String]
      val category       = c.downField(Constants.category).as[String]
      val filterProperty = c.downField(Constants.filterProperty).as[String]
      val domains        = c.downField(Constants.domains).as[Array[String]]
      val patterns       = c.downField(Constants.patterns).as[List[String]]
      val isSensitive    = c.downField(Constants.isSensitive).as[Boolean]
      val sensitivity    = c.downField(Constants.sensitivity).as[String]
      val tags           = c.downField(Constants.tags).as[Map[String, String]]
      Right(
        RuleInfo(
          id = id.getOrElse(""),
          name = name.getOrElse(""),
          category = category.getOrElse(""),
          filterProperty = FilterProperty.withNameWithDefault(filterProperty.getOrElse("")),
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

case class CpgWithOutputMap(cpg: Cpg, outputMap: Map[String, Json])
