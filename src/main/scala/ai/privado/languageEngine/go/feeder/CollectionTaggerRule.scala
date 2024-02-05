package ai.privado.languageEngine.go.feeder

import ai.privado.model.{CatLevelOne, FilterProperty, Language, NodeType, RuleInfo}

import scala.collection.immutable.HashMap

object CollectionTaggerRule {

  val rule =
    RuleInfo(
      "",
      "",
      "",
      FilterProperty.METHOD_FULL_NAME,
      Array.empty,
      List.empty,
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      "default",
      Language.GO,
      Array[String]()
    )
}
