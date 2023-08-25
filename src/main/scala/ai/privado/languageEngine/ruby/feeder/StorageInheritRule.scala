package ai.privado.languageEngine.ruby.feeder

import ai.privado.model.{CatLevelOne, Language, NodeType, RuleInfo}

import scala.collection.immutable.HashMap

object StorageInheritRule {

  val rules = List(
    RuleInfo(
      "Sink.DataBase.ROR.Read",
      "Ruby on Rails Repository(Read)",
      "",
      Array[String](),
      List[String](".*/models/.*", "(find.*|where|select)"),
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
      "Sink.DataBase.ROR.Write",
      "Ruby on Rails Repository(Write)",
      "",
      Array[String](),
      List[String](".*/models/.*", "save|update.*"),
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
      "Sink.DataBase.ROR.ReadAndWrite",
      "Ruby on Rails Repository(ReadAndWrite)",
      "",
      Array[String](),
      List[String](".*/models/.*", ""),
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
