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
import io.circe.{Decoder, HCursor}

import scala.collection.immutable.HashMap

case class DatabaseDetails(dbName: String, dbVendor: String, dbLocation: String, dbOperation: String)

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
  categoryTree: Array[String],
){
  def combinedRulePattern: String = {
    patterns.mkString("(", "|", ")")
  }
}
case class ConfigAndRules(
  sources: List[RuleInfo],
  sinks: List[RuleInfo],
  collections: List[RuleInfo],
  policies: List[PolicyOrThreat],
  threats: List[PolicyOrThreat],
  exclusions: List[RuleInfo],
  semantics: List[Semantic],
  sinkSkipList: List[RuleInfo]
)

case class DataFlow(sources: List[String], sinks: List[String])

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
  file: String,
  categoryTree: Array[String]
)

case class Semantic(
  signature: String,
  flow: String,
  file: String,
  language: Language.Language,
  categoryTree: Array[String]
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
      Right(
        PolicyOrThreat(
          id = id.getOrElse(""),
          name = name.getOrElse(""),
          fix = fix.getOrElse(""),
          policyOrThreatType = PolicyThreatType.withNameDefaultHandler(policyOrThreatType.getOrElse("")),
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
      Right(
        ConfigAndRules(
          sources = sources.getOrElse(List[RuleInfo]()),
          sinks = sinks.getOrElse(List[RuleInfo]()),
          collections = collections.getOrElse(List[RuleInfo]()),
          policies = policies.getOrElse(List[PolicyOrThreat]()),
          exclusions = exclusions.getOrElse(List[RuleInfo]()),
          threats = threats.getOrElse(List[PolicyOrThreat]()),
          semantics = semantics.getOrElse(List[Semantic]()),
          sinkSkipList = sinkSkipList.getOrElse(List[RuleInfo]())
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
