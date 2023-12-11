package ai.privado

import ai.privado.model.{CatLevelOne, ConfigAndRules, Language, NodeType, RuleInfo}

object RuleInfoTestData {

  val sourceRule = List(
    RuleInfo(
      "Data.Sensitive.FirstName",
      "FirstName",
      "",
      Array(),
      List("(?i).*firstName.*"),
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
    )
  )

  val rule: ConfigAndRules =
    ConfigAndRules(sourceRule, List(), List(), List(), List(), List(), List(), List(), List(), List())
}
