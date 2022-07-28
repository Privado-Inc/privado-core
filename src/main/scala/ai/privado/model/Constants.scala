package ai.privado.model

object Constants {
  // RuleInfo
  val id             = "id"
  val name           = "name"
  val category       = "category"
  val domains        = "domains"
  val nodeType       = "nodeType"
  val sensitivity    = "sensitivity"
  val isSensitive    = "isSensitive"
  val patterns       = "patterns"
  val privadoDerived = "privadoDerived"
  val underScore     = "_"

  val collections = "collections"
  val policies    = "policies"

  // Policy
  val description  = "description"
  val action       = "action"
  val dataFlow     = "dataFlow"
  val repositories = "repositories"

  // Source/sink types
  val dataElementSource = "DATA_ELEMENT"
  val storageSink       = "STORAGE"
  val leakageSink       = "LEAKAGE"
  val apiSink           = "API"
  val sharingSink       = "SHARING"

  // Source/sink key name
  val storage = "storage"
  val leakage = "leakage"
  val api     = "api"
  val sharing = "sharing"

  // Output keys
  val sourceType = "sourceType"

  val sinkType     = "sinkType"
  val sinks        = "sinks"
  val tags         = "tags"
  val fileName     = "fileName"
  val lineNumber   = "lineNumber"
  val columnNumber = "columnNumber"
  val sample       = "sample"
  val excerpt      = "excerpt"
  val sourceId     = "sourceId"
  val occurrences  = "occurrences"
  val paths        = "paths"
  val pathId       = "pathId"
  val path         = "path"
  val gitMetadata  = "gitMetadata"
  val branchName   = "branchName"
  val commitId     = "commitId"
  val remoteUrl    = "remoteUrl"

  val minusOne = "-1"

  val version       = "version"
  val createdAt     = "createdAt"
  val gitMetaData   = "gitMetaData"
  val sources       = "sources"
  val localScanPath = "localScanPath"
  val processing    = "processing"

}
