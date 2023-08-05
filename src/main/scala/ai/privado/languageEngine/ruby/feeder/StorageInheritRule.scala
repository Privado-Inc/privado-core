package ai.privado.languageEngine.ruby.feeder

import ai.privado.model.{CatLevelOne, Language, NodeType, RuleInfo}

import scala.collection.immutable.HashMap

object StorageInheritRule {

  val rules = List(
    RuleInfo(
      "Sink.DataBase.RWO.Read",
      "Ruby Model Repository(Read)",
      "",
      Array[String](),
      List[String](".*/model/.*", "(find|get|fetch)"),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "storages",
      Language.RUBY,
      Array[String]()
    ),
    RuleInfo(
      "Sink.DataBase.RWO.WRITE",
      "Ruby Model Repository(Write)",
      "",
      Array[String](),
      List[String](".*/model/.*", "(save|delete)"),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "storages",
      Language.RUBY,
      Array[String]()
    )
  )

}
