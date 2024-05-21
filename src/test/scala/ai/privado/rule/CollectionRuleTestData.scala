package ai.privado.rule

import ai.privado.model.{CatLevelOne, Constants, FilterProperty, Language, NodeType, RuleInfo}

object CollectionRuleTestData {

  val kotlinCollectionRule = List(
    RuleInfo(
      "Collections.Kotlin.Annotation",
      "Annotation",
      "",
      FilterProperty.CODE,
      Array(),
      List(".*(GET|PUT|POST|DELETE|HEAD|OPTIONS).*"),
      false,
      "",
      Map(),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      catLevelTwo = Constants.annotations,
      Language.KOTLIN,
      Array()
    )
  )

}
