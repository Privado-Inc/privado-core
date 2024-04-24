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
  val id                           = "id"
  val name                         = "name"
  val category                     = "category"
  val filterProperty               = "filterProperty"
  val domains                      = "domains"
  val nodeType                     = "nodeType"
  val catLevelOne                  = "catLevelOne"
  val catLevelTwo                  = "catLevelTwo"
  val sensitivity                  = "sensitivity"
  val isSensitive                  = "isSensitive"
  val patterns                     = "patterns"
  val privadoDerived               = "privadoDerived"
  val underScore                   = "_"
  val apiUrl                       = "apiUrl"
  val collectionSource             = "collectionSource"
  val android                      = "android"
  val monolithRepoItem             = "monolithRepoItem"
  val monolithRepoReachingFileList = "monolithRepoReachingFileList"
  val monolithJsonPath             = "monolithJsonPath"
  val monolithRepoDepth            = "monolithRepoDepth"

  // semantic
  val signature = "signature"
  val flow      = "flow"

  // systemConfig
  val key                       = "key"
  val value                     = "value"
  val MAX_SOCKET_COUNT          = "maxSocketCount"
  val RepoPropertyConfig        = "RepoPropertyConfig"
  val filePath                  = "filePath"
  val PropertyFileSizeLimit     = "propertyFileSizeLimit"
  val PropertyFileDirCountLimit = "propertyFileDirCountLimit"
  val MaxCharLimit              = "maxCharLimit"

  val third_parties      = "third_parties"
  val internal_apis      = "internal_apis"
  val collections        = "collections"
  val leakages           = "leakages"
  val storages           = "storages"
  val policies           = "policies"
  val exclusions         = "exclusions"
  val threats            = "threats"
  val semantics          = "semantics"
  val sinkSkipList       = "sinkSkipList"
  val systemConfig       = "systemConfig"
  val auditConfig        = "auditConfig"
  val auditCollection    = "collections"
  val auditWebClient     = "webClient"
  val auditUtility       = "utility"
  val androidPermissions = "androidPermissions"

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

  val config                        = "config"
  val coreVersion                   = "privadoCoreVersion"
  val cliVersion                    = "privadoCLIVersion"
  val mainVersion                   = "privadoMainVersion"
  val notDetected                   = "Not Detected"
  val createdAt                     = "createdAt"
  val repoName                      = "repoName"
  val language                      = "language"
  val gitMetaData                   = "gitMetaData"
  val sources                       = "sources"
  val sourceFilters                 = "sourceFilters"
  val sinkFilters                   = "sinkFilters"
  val allowedSourceFilters          = "allowedSourceFilters"
  val allowedSinkFilters            = "allowedSinkFilters"
  val sinkType                      = "sinkType"
  val collectionFilters             = "collectionFilters"
  val collectionType                = "collectionType"
  val inferences                    = "inferences"
  val localScanPath                 = "localScanPath"
  val processing                    = "processing"
  val sinkProcessing                = "sinkProcessing"
  val probableSinks                 = "probableSinks"
  val violations                    = "violations"
  val ingressUrls                   = "ingressUrls"
  val egressUrls                    = "egressUrls"
  val egressUrlsFromCode            = "egressUrlsFromCode"
  val httpEndPointBasePaths         = "httpEndPointBasePaths"
  val outputFileName                = "privado.json"
  val outputDirectoryName           = ".privado"
  val generatedConfigFolderName     = ".privado/config"
  val outputIntermediateFileName    = "intermediate.json"
  val privadoLanguageEngineVersion  = "privadoLanguageEngineVersion"
  val cpgOutputFileName             = "cpg.bin"
  val outputAuditFileName           = "audit-report.xlsx"
  val outputSemanticFileName        = "semantic.txt"
  val outputUnresolvedFilename      = "unresolved-flow.json"
  val repoConfigMetaData            = "RepoConfigMetaData"
  val propertyFileSkippedBySize     = "PropertyFileSkippedBySize"
  val propertyFileSkippedByDirCount = "PropertyFileSkippedByDirCount"
  val arguments                     = "arguments"

  // database details
  val dbName      = "dbName"
  val dbVendor    = "dbVendor"
  val dbLocation  = "dbLocation"
  val dbOperation = "dbOperation"
  val schema      = "schema"

  // Other commonly used constants
  val RULES_DIR_IN_CONFIG           = "rules"
  val CONFIG_DIR_IN_CONFIG          = "config"
  val PRETTY_LINE_SEPARATOR: String = "-" * 100
  val EMPTY                         = "<empty>"
  val READ_WITH_BRACKETS            = " (Read)"
  val WRITE_WITH_BRACKETS           = " (Write)"
  val BYTE_ENCODING_CODE            = "ISO-8859-1"

  // Stat files
  val JAVA_STATS   = "java.txt"
  val JS_STATS     = "js.txt"
  val PYTHON_STATS = "python.txt"
  val RUBY_STATS   = "ruby.txt"

  val delombok = "delombok"
  val API      = "API"

  // Rule ids
  val internalAPIRuleId            = "Sinks.API.InternalAPI"
  val thirdPartiesAPIRuleId        = "Sinks.ThirdParties.API"
  val jmsConsumerRuleId            = "Messaging.Service.JMS.Consumer"
  val kafkaConsumerRuleId          = "Messaging.Queue.Kafka.Consumer"
  val cookieWriteRuleId            = "Storages.Web.Cookie.Write"
  val googleTagManagerPixelRuleId  = "ThirdParties.SDK.Pixel.Google.TagManager"
  val segmentAnalyticsRuleId       = "ThirdParties.SDK.Segment.Analytics"
  val segmentPixelRuleId           = "ThirdParties.SDK.Pixel.Segment"
  val googleTagManagerRuleId       = "ThirdParties.SDK.Google.TagManager"
  val cookieSourceRuleId           = "Data.Sensitive.OnlineIdentifiers.Cookies"
  val ignoredSinks                 = "ignoredSinks"
  val apiSinks                     = "apiSinks"
  val apiMethodFullNames           = "apiMethodFullNames"
  val apiHttpLibraries             = "apiHttpLibraries"
  val apiIdentifier                = "apiIdentifier"
  val apiGraphqlLibraries          = "apiGraphqlLibraries"
  val apiGraphqlReadSink           = "apiGraphqlReadSink"
  val apiGraphqlWriteSink          = "apiGraphqlWriteSink"
  val clientCreationBaseUrlPattern = "clientCreationBaseUrlPattern"

  // External script keys
  val postExportTrigger = "postExportTrigger"

  // TemplatedDom

  val jsxOpenElement = "JSXOpeningElement"
  val jsxElement     = "JSXElement"

  val HTMLElement          = "HTMLElement"
  val HTMLOpenElement      = "HTMLOpenElement"
  val HTMLClosingElement   = "HTMLClosingElement"
  val HTMLElementAttribute = "HTMLElementAttribute"
  val HTMLScriptElement    = "HTMLScriptElement"

  val UnknownDomain = "unknown-domain"
  val Unknown       = "unknown"

  // catlevelTwo
  val annotations       = "annotations"
  val default           = "default"
  val apiEndpoint       = "apiEndpoint"
  val semanticDelimeter = "_A_"
  val thisConstant      = "this"

  // Ruby defaults
  val defaultExpansionLimit = 100

  val defaultLineNumber = -1

  // operator
  val scopeResolutionOperator = "<operator>.scopeResolution"

  // Routes folder, or file name
  val routes = "routes"
}
