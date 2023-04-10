package ai.privado.audit

object AuditReportConstants {

  val AUDIT_ELEMENT_DISCOVERY_SHEET_NAME = "Element-Discovery Audit"

  val AUDIT_DEPENDENCY_SHEET_NAME = "Dependency Audit"

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

  val DEPENDENCY_LIBRARY_NAME = "Library ( GroupId - ArtifactId)"

  val DEPENDENCY_ARTIFACT_NAME = "ArtifactID"

  val DEPENDENCY_FILE_PATH_NAME = "File Path"

  val DEPENDENCY_PROCESSED_NAME = "Being processed"

  val DEPENDENCY_CATEGORY_NAME = "Category"

  val DEPENDENCY_MATCHING_RULE_NAME = "Matching rule Id"

}
