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
  val API            = Value("api")
  val LEAKAGE        = Value("Leakage")
  val SDK            = Value("SDK")
  val REGULAR        = Value("REGULAR")

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

  def withNameWithDefault(name: String): CatLevelOneIn = {
    try {
      withName(name).asInstanceOf[CatLevelOne.CatLevelOneIn]
    } catch {
      case _: Throwable => this.UNKNOWN
    }
  }
}

object CatLevelTwo extends Enumeration {
  type CatLevelTwo = CatLevelTwoIn

  case class CatLevelTwoIn(name: String, label: String) extends Val(name)

  val STORAGES      = CatLevelTwoIn("storages", "Storages")
  val INTERNAL_APIS = CatLevelTwoIn("internal_apis", "Internal APIs")
  val LEAKAGES      = CatLevelTwoIn("leakages", "Leakages")
  val THIRD_PARTIES = CatLevelTwoIn("third_parties", "Third Parties")
  val UNKNOWN       = CatLevelTwoIn("unknown", "Unknown")

  def withNameWithDefault(name: String): CatLevelTwoIn = {
    try {
      withName(name).asInstanceOf[CatLevelTwo.CatLevelTwoIn]
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
