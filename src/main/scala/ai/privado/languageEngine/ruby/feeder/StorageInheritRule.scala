package ai.privado.languageEngine.ruby.feeder

import ai.privado.model.{CatLevelOne, FilterProperty, Language, NodeType, RuleInfo}

import scala.collection.immutable.HashMap

object StorageInheritRule {

  val rules = List(
    RuleInfo(
      "Sink.DataBase.ROR.Read",
      "Ruby on Rails Repository(Read)",
      "",
      FilterProperty.METHOD_FULL_NAME,
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
      FilterProperty.METHOD_FULL_NAME,
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
      FilterProperty.METHOD_FULL_NAME,
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
