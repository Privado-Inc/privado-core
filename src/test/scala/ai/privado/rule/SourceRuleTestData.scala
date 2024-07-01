package ai.privado.rule

import ai.privado.model.{CatLevelOne, FilterProperty, Language, NodeType, RuleInfo, Constants}

object SourceRuleTestData {

  val firstNameSourceRule = RuleInfo(
    "Data.Sensitive.FirstName",
    "FirstName",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array(),
    List("(?i)firstName|first_name"),
    false,
    "",
    Map(),
    NodeType.REGULAR,
    "",
    CatLevelOne.SOURCES,
    "",
    Language.UNKNOWN,
    Array()
  )

  val userNameSourceRule = RuleInfo(
    "Data.Sensitive.User",
    "User",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array(),
    List("(?i)(.*userName.*)"),
    false,
    "",
    Map(),
    NodeType.REGULAR,
    "",
    CatLevelOne.SOURCES,
    catLevelTwo = Constants.default,
    Language.UNKNOWN,
    Array()
  )

  val accountPasswordSourceRule = RuleInfo(
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
    Language.UNKNOWN,
    Array()
  )

  val lastNameSourceRule = RuleInfo(
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
  )

  val dobSourceRule = RuleInfo(
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
  )
  val emailSourceRule = RuleInfo(
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
  )

  val phoneNumberSourceRule = RuleInfo(
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
  )
  val salarySourceRule = RuleInfo(
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
}
