package ai.privado.rule

import ai.privado.model.{DEDClassificationData, DEDRuleInfo, DEDVariable}

object DEDRuleTestData {
  val filepath = "Test0.js"
  val classificationData = List(
    DEDClassificationData(
      id = "Data.Sensitive.AccountData.AccountPassword",
      variables = List(DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(-1)))
    ),
    DEDClassificationData(
      id = "DISABLED_BY_DED",
      variables = List(DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(-1)))
    )
  )
  val dedRuleTestJS = DEDRuleInfo(id = filepath, filePath = filepath, classificationData = classificationData)

  val dedRuleTestJava = DEDRuleInfo(id = "User.java", filePath = "User.java", classificationData = classificationData)

  val dedRuleTestKotlin = DEDRuleInfo(
    id = "extraKotlinFolder/Test0.kt",
    filePath = "extraKotlinFolder/Test0.kt",
    classificationData = classificationData
  )
}
