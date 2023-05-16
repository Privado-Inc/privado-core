package ai.privado.audit

object AuditReportConstants {

  val AUDIT_ELEMENT_DISCOVERY_SHEET_NAME = "Element-Discovery Audit"

  val AUDIT_DEPENDENCY_SHEET_NAME = "Dependency Audit"

  val AUDIT_DATA_FLOW_SHEET_NAME = "Data Flow Audit"

  val AUDIT_UNRESOLVED_SHEET_NAME = "Unresolved Flow"

  val AUDIT_EMPTY_CELL_VALUE = "--"

  val AUDIT_CHECKED_VALUE = "YES"

  val AUDIT_NOT_CHECKED_VALUE = "NO"

  val AUDIT_BUILT_IN_CLASS_REGEX =
    "\\b((int|byte|short|long|float|double|boolean|char)|((java\\.|javax\\.|org\\.|com\\.sun\\.|com\\.oracle\\.)([A-Z][a-zA-Z0-9_]*(\\.[A-Z][a-zA-Z0-9_]*)*)))\\b"

  val ELEMENT_DISCOVERY_CLASS_NAME = "Class"

  val ELEMENT_DISCOVERY_FILE_NAME = "File Name"

  val ELEMENT_DISCOVERY_MEMBER_NAME = "Member"

  val ELEMENT_DISCOVERY_MEMBER_TYPE = "Member Type"

  val ELEMENT_DISCOVERY_TAGGED_NAME = "Tagged"

  val ELEMENT_DISCOVERY_SOURCE_RULE_ID = "Source Rule ID"

  val ELEMENT_DISCOVERY_INPUT_COLLECTION = "Input to Collection"

  val ELEMENT_DISCOVERY_COLLECTION_ENDPOINT = "Collection Endpoint Path"

  val ELEMENT_DISCOVERY_METHOD_NAME = "Collection Method Full Name"

  val ELEMENT_DISCOVERY_EXCLUDE_CLASS_NAME_REGEX = "^(.*)(Controller|Service|Impl|Helper|Util|Processor|Dao)$"

  val ELEMENT_DISCOVERY_GET_SET_METHOD_REGEX = "^(get|set).*"

  val ELEMENT_DISCOVERY_OVERRIDE_METHOD_REGEX = "^(hascode|equals)"

  val ELEMENT_DISCOVERY_GETTER_SETTER_REGEX = ".*(Getter|Setter).*"

  val DATA_FLOW_SOURCE_NAME = "Source"

  val DATA_FLOW_SINK_NAME = "Sink"

  val DATA_FLOW_FLOW_NAME = "Flow Id"

  val DATA_FLOW_SHORT_FLOW_NAME = "Short Flow"

  val DATA_FLOW_JOERN_OUTPUT_NAME = "Joern Output"

  val DATA_FLOW_SEMANTIC_FILTER_NAME = "Semantic Filter"

  val DATA_FLOW_FILTER_1_NAME = "FP - this Filtering"

  val DATA_FLOW_FILTER_2_NAME = "FP - Separation b/w Data Elements"

  val DATA_FLOW_DEDUP_1_NAME = "Deduplication of Sub Flow (subset)"

  val DATA_FLOW_DEDUP_2_NAME = "Grouping by Same Line No. "

  val DATA_FLOW_FINAL_RESULT_NAME = "Final Result"

  val DEPENDENCY_LIBRARY_NAME = "Library ( GroupId - ArtifactId)"

  val DEPENDENCY_ARTIFACT_NAME = "ArtifactID"

  val DEPENDENCY_FILE_PATH_NAME = "File Path"

  val DEPENDENCY_PROCESSED_NAME = "Being processed"

  val DEPENDENCY_CATEGORY_NAME = "Category"

  val DEPENDENCY_MATCHING_RULE_NAME = "Matching rule Id"

  val DEPENDENCY_INTERNAL_LIBRARY_NAME = "Internal"

  val DEPENDENCY_KNOW_THIRD_PARTY_LIBRARY_NAME = "Know Third Party"

  val DEPENDENCY_STORAGE_LIBRARY_NAME = "Storage"

  val DEPENDENCY_LEAKAGE_LIBRARY_NAME = "Leakage"

  val DEPENDENCY_COLLECTION_LIBRARY_NAME = "Collection"

  val DEPENDENCY_WEB_CLIENT_LIBRARY_NAME = "Web Client"

  val DEPENDENCY_UTILITY_LIBRARY_NAME = "Utility"

  val DEPENDENCY_UNKNOWN_LIBRARY_NAME = "Unknown"

  val DEPENDENCY_UNRESOLVED_SOURCE_NAME = "Source"

  val DEPENDENCY_UNRESOLVED_SINK_NAME = "Sink (methodFullName)"

  val DEPENDENCY_UNRESOLVED_FLOW_ID_NAME = "flowId"

  val DEPENDENCY_UNRESOLVED_CODE_SNIPPET_NAME = "Code Snippet"
}
