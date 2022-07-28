package ai.privado.model

object InternalTags extends Enumeration {

  type InternalTags = Value

  val VARIABLE_REGEX_LITERAL                   = Value("VARIABLE_REGEX_LITERAL")
  val VARIABLE_REGEX_IDENTIFIER                = Value("VARIABLE_REGEX_IDENTIFIER")
  val OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME = Value("OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_NAME")
  val OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE = Value("OBJECT_OF_SENSITIVE_CLASS_BY_INHERITANCE")
  val OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE = Value("OBJECT_OF_SENSITIVE_CLASS_BY_MEMBER_TYPE")
  val SENSITIVE_FIELD_ACCESS                   = Value("SENSITIVE_FIELD_ACCESS")
  val PRIVADO_DERIVED                          = Value("PRIVADO_DERIVED")

  val API_URL = Value("API_URL")

  lazy val valuesAsString = InternalTags.values.map(value => value.toString())

}

object NodeType extends Enumeration {

  type NodeType = Value

  val SOURCE         = Value("Source")
  val DERIVED_SOURCE = Value("DerivedSource")
  val DATABASE       = Value("Database")
  val API            = Value("API")
  val LEAKAGE        = Value("Leakage")
  val SDK            = Value("SDK")
  val COLLECTIONS    = Value("Collections")
  val UNKNOWN        = Value("Unknown")

  def withNameWithDefault(name: String): Value = {
    try {
      withName(name)
    } catch {
      case _: Throwable => this.UNKNOWN
    }
  }
}
