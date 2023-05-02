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

object InternalTag extends Enumeration {

  type InternalTag = Value

  val VARIABLE_REGEX_LITERAL                   = Value("VARIABLE_REGEX_LITERAL")
  val VARIABLE_REGEX_IDENTIFIER                = Value("VARIABLE_REGEX_IDENTIFIER")
  val VARIABLE_REGEX_MEMBER                    = Value("VARIABLE_REGEX_MEMBER")
  val OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME = Value("OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME")
  val OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE = Value("OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE")
  val OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE = Value("OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE")
  val SENSITIVE_FIELD_ACCESS                   = Value("SENSITIVE_FIELD_ACCESS")
  val COLLECTION_METHOD_ENDPOINT               = Value("COLLECTION_METHOD_ENDPOINT")
  val SENSITIVE_METHOD_RETURN                  = Value("SENSITIVE_METHOD_RETURN")
  val INDEX_ACCESS_CALL                        = Value("INDEX_ACCESS_CALL")
  val INSENSITIVE_METHOD_RETURN                = Value("INSENSITIVE_METHOD_RETURN")
  val INSENSITIVE_FIELD_ACCESS                 = Value("INSENSITIVE_FIELD_ACCESS")
  val INSENSITIVE_SETTER                       = Value("INSENSITIVE_SETTER")
  val SENSITIVE_SETTER                         = Value("SENSITIVE_SETTER")

  lazy val valuesAsString = InternalTag.values.map(value => value.toString())

}

object NodeType extends Enumeration {

  type NodeType = Value

  val API     = Value("api")
  val REGULAR = Value("REGULAR")
  val UNKNOWN = Value("Unknown")

  def withNameWithDefault(name: String): Value = {
    try {
      withName(name)
    } catch {
      case _: Throwable => this.REGULAR
    }
  }
}

object CatLevelOne extends Enumeration {
  type CatLevelOne = CatLevelOneIn

  case class CatLevelOneIn(name: String, label: String) extends Val(name)

  val SOURCES     = CatLevelOneIn("sources", "Data Element")
  val SINKS       = CatLevelOneIn("sinks", "Sinks")
  val COLLECTIONS = CatLevelOneIn("collections", "Collections")
  val POLICIES    = CatLevelOneIn("policies", "Policies")
  val THREATS     = CatLevelOneIn("threats", "Threats")
  val UNKNOWN     = CatLevelOneIn("unknown", "Unknown")

  // internal CatLevelOne
  val DERIVED_SOURCES = CatLevelOneIn("DerivedSources", "Data Element")

  def withNameWithDefault(name: String): CatLevelOneIn = {
    try {
      withName(name).asInstanceOf[CatLevelOne.CatLevelOneIn]
    } catch {
      case _: Throwable => this.UNKNOWN
    }
  }
}

object Language extends Enumeration {
  type Language = Value

  val JAVA       = Value("java")
  val JAVASCRIPT = Value("javascript")
  val PYTHON     = Value("python")
  val DEFAULT    = Value("default")
  val UNKNOWN    = Value("unknown")
  def withNameWithDefault(name: String): Value = {
    try {
      withName(name)
    } catch {
      case _: Throwable => this.UNKNOWN
    }
  }
}

object PolicyAction extends Enumeration {
  type PolicyAction = Value

  val ALLOW = Value("allow")
  val DENY  = Value("deny")

  def withNameDefaultHandler(name: String): Value = {
    if (name != null)
      try {
        withName(name.toLowerCase())
      } catch {
        case _: Throwable => null
      }
    else
      null
  }

}
object PolicyThreatType extends Enumeration {
  type PolicyThreatType = Value

  val THREAT     = Value("threat")
  val COMPLIANCE = Value("compliance")

  def withNameDefaultHandler(name: String): Value = {
    if (name != null)
      try {
        withName(name.toLowerCase())
      } catch {
        case _: Throwable => null
      }
    else
      null
  }
}

object ConfigRuleType extends Enumeration {
  type ConfigRuleType = Value

  val EXCLUSIONS     = Value("exclusions")
  val SEMANTICS      = Value("semantics")
  val SINK_SKIP_LIST = Value("sinkSkipList")
  val SYSTEM_CONFIG  = Value("systemConfig")
  val AUDIT_CONFIG   = Value("auditConfig")

  def withNameDefaultHandler(name: String): Value = {
    if (name != null)
      try {
        withName(name)
      } catch {
        case _: Throwable => null
      }
    else
      null
  }
}
