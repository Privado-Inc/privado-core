package ai.privado.model

object InternalTag extends Enumeration {

  type InternalTag = Value

  val VARIABLE_REGEX_LITERAL                   = Value("VARIABLE_REGEX_LITERAL")
  val VARIABLE_REGEX_IDENTIFIER                = Value("VARIABLE_REGEX_IDENTIFIER")
  val OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME = Value("OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME")
  val OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE = Value("OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE")
  val OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE = Value("OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE")
  val SENSITIVE_FIELD_ACCESS                   = Value("SENSITIVE_FIELD_ACCESS")

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

  val JAVA    = Value("java")
  val UNKNOWN = Value("unknown")
  def withNameWithDefault(name: String): Value = {
    try {
      withName(name)
    } catch {
      case _: Throwable => this.UNKNOWN
    }
  }
}
