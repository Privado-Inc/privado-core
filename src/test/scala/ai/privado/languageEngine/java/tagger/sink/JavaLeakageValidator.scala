package ai.privado.languageEngine.java.tagger.sink

import ai.privado.model.{CatLevelOne, Constants, FilterProperty, Language, NodeType, RuleInfo}
import ai.privado.tagger.sink.LeakageValidator

trait JavaLeakageValidator extends LeakageValidator {

  val leakageRule: RuleInfo = RuleInfo(
    "Leakages.Log.Console",
    "Log Console",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array(),
    List("(?i)(java.io.PrintStream|android.util.(?:Log){0,1}Printer).(?:print|println|write).*"),
    false,
    "",
    Map(),
    NodeType.REGULAR,
    "",
    CatLevelOne.SINKS,
    catLevelTwo = Constants.leakages,
    Language.JAVA,
    Array()
  )
}
