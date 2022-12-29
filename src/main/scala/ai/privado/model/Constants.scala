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

object Constants {
  // RuleInfo
  val id             = "id"
  val name           = "name"
  val category       = "category"
  val domains        = "domains"
  val nodeType       = "nodeType"
  val catLevelOne    = "catLevelOne"
  val catLevelTwo    = "catLevelTwo"
  val sensitivity    = "sensitivity"
  val isSensitive    = "isSensitive"
  val patterns       = "patterns"
  val privadoDerived = "privadoDerived"
  val underScore     = "_"
  val apiUrl         = "apiUrl"

  // semantic
  val signature = "signature"
  val flow      = "flow"

  val third_parties = "third_parties"
  val internal_apis = "internal_apis"
  val collections   = "collections"
  val leakages      = "leakages"
  val storages      = "storages"
  val policies      = "policies"
  val exclusions    = "exclusions"
  val threats       = "threats"
  val semantics     = "semantics"
  val sinkSkipList  = "sinkSkipList"

  val tags               = "tags"
  val description        = "description"
  val action             = "action"
  val dataFlow           = "dataFlow"
  val repositories       = "repositories"
  val policyOrThreatType = "type"
  val fix                = "fix"
  val sinks              = "sinks"

  val gitMetadata = "gitMetadata"
  val branchName  = "branchName"
  val commitId    = "commitId"
  val remoteUrl   = "remoteUrl"
  val endPoint    = "endPoint"

  val coreVersion         = "privadoCoreVersion"
  val cliVersion          = "privadoCLIVersion"
  val mainVersion         = "privadoMainVersion"
  val notDetected         = "Not Detected"
  val createdAt           = "createdAt"
  val repoName            = "repoName"
  val gitMetaData         = "gitMetaData"
  val sources             = "sources"
  val localScanPath       = "localScanPath"
  val processing          = "processing"
  val sinkProcessing      = "sinkProcessing"
  val probableSinks       = "probableSinks"
  val outputFileName      = "privado.json"
  val outputDirectoryName = ".privado"

  // database details
  val dbName      = "dbName"
  val dbVendor    = "dbVendor"
  val dbLocation  = "dbLocation"
  val dbOperation = "dbOperation"

  // Other commonly used constants
  val RULES_DIR_IN_CONFIG           = "rules"
  val CONFIG_DIR_IN_CONFIG          = "config"
  val PRETTY_LINE_SEPARATOR: String = "-" * 100
  val EMPTY                         = "<empty>"
}
