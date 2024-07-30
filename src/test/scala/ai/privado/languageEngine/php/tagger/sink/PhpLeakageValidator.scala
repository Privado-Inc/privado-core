package ai.privado.languageEngine.php.tagger.sink

import ai.privado.tagger.sink.LeakageValidator
import ai.privado.model.{CatLevelOne, Constants, FilterProperty, NodeType, RuleInfo}
import ai.privado.model.Language

trait PhpLeakageValidator extends LeakageValidator {
  val leakageRule = RuleInfo(
    "Log console",
    "Leakage",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array(),
    List(".*log->info.*"),
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
