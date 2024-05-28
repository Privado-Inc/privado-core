package ai.privado.rule

import ai.privado.model.{DEDClassificationData, DEDRuleInfo, DEDVariable}

object DEDRuleTestData {
  val filepath = "Test0.js"
  val dedRuleTestJS = DEDRuleInfo(
    id = filepath,
    filePath = filepath,
    classificationData = List(
      DEDClassificationData(
        id = "Data.Sensitive.AccountData.AccountPassword",
        variables = List(DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(-1)))
      ),
      DEDClassificationData(
        id = "DISABLED_BY_DED",
        variables = List(DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(-1)))
      )
    )
  )
}
