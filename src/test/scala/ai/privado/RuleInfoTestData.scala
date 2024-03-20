package ai.privado

import ai.privado.model.{CatLevelOne, ConfigAndRules, Constants, FilterProperty, Language, NodeType, RuleInfo}

object RuleInfoTestData {

  val sourceRule = List(
    RuleInfo(
      "Data.Sensitive.FirstName",
      "FirstName",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*firstName|first_name.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.JAVA,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.AccountData.AccountPassword",
      "AccountPassword",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*password.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.JAVA,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.PersonalIdentification.LastName",
      "LastName",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*lastName.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.PersonalIdentification.DateofBirth",
      "Date of Birth",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*dob.*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.ContactData.EmailAddress",
      "EmailAddress",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*email.*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.ContactData.PhoneNumber",
      "Phone",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*phone.*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    ),
    RuleInfo(
      "Data.Sensitive.FinancialData.Salary",
      "Salary",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List("(?i).*salary.*"),
      true,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.SOURCES,
      "",
      Language.UNKNOWN,
      Array()
    )
  )

  val apiLiteralRule = List(
    RuleInfo(
      Constants.thirdPartiesAPIRuleId,
      "Third Party API",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array(),
      List(
        "((?i)((?:http:|https:|ftp:|ssh:|udp:|wss:){0,1}(\\/){0,2}[a-zA-Z0-9_-][^)\\/(#|,!>\\s]{1,50}\\.(?:com|net|org|de|in|uk|us|io|gov|cn|ml|ai|ly|dev|cloud|me|icu|ru|info|top|tk|tr|cn|ga|cf|nl)).*(?<!png|jpeg|jpg|txt|blob|css|html|js|svg))"
      ),
      false,
      "",
      Map(),
      NodeType.API,
      "",
      CatLevelOne.SINKS,
      catLevelTwo = Constants.third_parties,
      Language.UNKNOWN,
      Array()
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(
      sources = sourceRule,
      sinks = apiLiteralRule,
      List(),
      List(),
      List(),
      List(),
      List(),
      List(),
      List(),
      List()
    )
}
