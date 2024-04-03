package ai.privado.rule

import ai.privado.model.{CatLevelOne, Constants, FilterProperty, Language, NodeType, RuleInfo}

object SinkRuleTestData {

  val thirdPartyAPIRule = RuleInfo(
    Constants.thirdPartiesAPIRuleId,
    "Third Party API",
    "",
    FilterProperty.METHOD_FULL_NAME,
    Array(),
    List(
      "((?i)((?:http:|https:|ftp:|ssh:|udp:|wss:){0,1}(\\/){0,2}[a-zA-Z0-9_-][^)\\/(#|,!>\\s]{1,50}\\.(?:com|net|org|de|in|uk|us|io|gov|cn|ml|ai|ly|dev|cloud|me|icu|ru|info|top|tk|tr|cn|ga|cf|nl)).*(?<!png|jpeg|jpg|txt|blob|css|html|js|svg))"
    ),
    false,
    "",
    Map(),
    NodeType.API,
    "",
    CatLevelOne.SINKS,
    catLevelTwo = Constants.third_parties,
    Language.UNKNOWN,
    Array()
  )

}
