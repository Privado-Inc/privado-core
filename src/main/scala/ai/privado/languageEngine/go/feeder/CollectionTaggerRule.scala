package ai.privado.languageEngine.go.feeder

import ai.privado.model.{CatLevelOne, Language, NodeType, RuleInfo}

import scala.collection.immutable.HashMap

object CollectionTaggerRule {

  val rule =
    RuleInfo(
      "",
      "",
      "",
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
