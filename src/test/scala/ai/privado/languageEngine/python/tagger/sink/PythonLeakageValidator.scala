package ai.privado.languageEngine.python.tagger.sink

import ai.privado.model.{CatLevelOne, Constants, FilterProperty, Language, NodeType, RuleInfo}
import ai.privado.tagger.sink.LeakageValidator

trait PythonLeakageValidator extends LeakageValidator {
  val leakageRule = RuleInfo(
    "Log console",
    "Leakage",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array(),
    List(".*print.*"),
    false,
    "",
    Map(),
    NodeType.REGULAR,
    "",
    CatLevelOne.SINKS,
    catLevelTwo = Constants.leakages,
    Language.UNKNOWN,
    Array()
  )
}
