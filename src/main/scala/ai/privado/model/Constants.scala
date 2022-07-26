package ai.privado.model

object Constants {

  val id          = "id"
  val name        = "name"
  val category    = "category"
  val nodeType    = "nodeType"
  val sensitivity = "sensitivity"
  val isSensitive = "isSensitive"
  val patterns    = "patterns"

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

  val minusOne = "-1"

  val version       = "version"
  val createdAt     = "createdAt"
  val gitMetaData   = "gitMetaData"
  val sources       = "sources"
  val localScanPath = "localScanPath"
  val processing    = "processing"

}
