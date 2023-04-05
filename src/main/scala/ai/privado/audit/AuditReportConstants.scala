package ai.privado.audit

object AuditReportConstants {

  val AUDIT_ELEMENT_DISCOVERY_SHEET_NAME = "Element-Discovery"

  val AUDIT_EMPTY_CELL_VALUE = "--"

  val AUDIT_CHECKED_VALUE = "YES"

  val AUDIT_NOT_CHECKED_VALUE = "NO"

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

}
