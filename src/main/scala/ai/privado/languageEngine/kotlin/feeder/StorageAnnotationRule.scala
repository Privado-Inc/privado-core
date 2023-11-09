package ai.privado.languageEngine.kotlin.feeder

import ai.privado.model.{CatLevelOne, Constants, Language, NodeType, RuleInfo}

import scala.collection.immutable.HashMap

object StorageAnnotationRule {

  val rules: List[RuleInfo] = List(
    RuleInfo(
      "Sinks.Database.Persistence.Room.Read",
      "Android Persistence Room(Read)",
      "",
      Array[String]("android.com"),
      List[String]("Dao", "(?i)(find|get|select|search|all|load).*"),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      Constants.storages,
      Language.KOTLIN,
      Array[String]()
    ),
    RuleInfo(
      "Sinks.Database.Persistence.Room.Write",
      "Android Persistence Room(Write)",
      "",
      Array[String]("android.com"),
      List[String]("Dao", "(?i)(save|delete|insert|update).*"),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      Constants.storages,
      Language.KOTLIN,
      Array[String]()
    )
  )
}
