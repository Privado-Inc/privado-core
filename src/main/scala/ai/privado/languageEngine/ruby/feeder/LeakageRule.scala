package ai.privado.languageEngine.ruby.feeder
import ai.privado.model.{CatLevelOne, Language, NodeType, RuleInfo}
import scala.collection.immutable.HashMap

object LeakageRule {
  val rules = List(
    RuleInfo(
      "Sink.Leakages.Log.Info",
      "Log Info",
      "",
      Array[String](),
      List[String]("log.*", "info"),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "leakages",
      Language.RUBY,
      Array[String]()
    ),
    RuleInfo(
      "Sink.Leakages.Log.Error",
      "Log Error",
      "",
      Array[String](),
      List[String]("log.*", "error"),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "leakages",
      Language.RUBY,
      Array[String]()
    ),
    RuleInfo(
      "Sink.Leakages.Log.Debug",
      "Log Debug",
      "",
      Array[String](),
      List[String]("log.*", "debug"),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "leakages",
      Language.RUBY,
      Array[String]()
    )
  )

}
