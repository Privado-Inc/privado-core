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
        variables = List(
          DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(4)),
          DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(6)),
          DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(19)),
          DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(21)),
          DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(22))
        )
      ),
      DEDClassificationData(
        id = "DISABLED_BY_DED",
        variables = List(
          DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(4)),
          DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(7)),
          DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(19)),
          DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(21)),
          DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(22))
        )
      )
    )
  )

  val dedRuleTestJava = DEDRuleInfo(
    id = "User.java",
    filePath = "User.java",
    classificationData = List(
      DEDClassificationData(
        id = "Data.Sensitive.AccountData.AccountPassword",
        variables = List(DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(4)))
      ),
      DEDClassificationData(
        id = "DISABLED_BY_DED",
        variables = List(DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(5)))
      )
    )
  )

  val dedRuleTestCSharp = DEDRuleInfo(
    id = "Test0.cs",
    filePath = "Test0.cs",
    classificationData = List(
      DEDClassificationData(
        id = "Data.Sensitive.AccountData.AccountPassword",
        variables = List(
          DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(7)),
          DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(10)),
          DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(13)),
          DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(18))
        )
      ),
      DEDClassificationData(
        id = "DISABLED_BY_DED",
        variables = List(
          DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(8)),
          DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(10)),
          DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(14)),
          DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(18)),
          DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(19))
        )
      )
    )
  )

  val dedRuleTestGolang = DEDRuleInfo(
    id = "generalFile.go",
    filePath = "generalFile.go",
    classificationData = List(
      DEDClassificationData(
        id = "Data.Sensitive.AccountData.AccountPassword",
        variables = List(
          DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(11)),
          DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = Some(21))
        )
      ),
      DEDClassificationData(
        id = "DISABLED_BY_DED",
        variables = List(
          DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(10)),
          DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = Some(20))
        )
      )
    )
  )

  val dedRuleTestKotlin = DEDRuleInfo(
    id = "extraKotlinFolder/Test0.kt",
    filePath = "extraKotlinFolder/Test0.kt",
    classificationData = List(
      DEDClassificationData(
        id = "Data.Sensitive.AccountData.AccountPassword",
        variables = List(DEDVariable(name = "passwd", typeInSrc = "ANY", lineNumber = None))
      ),
      DEDClassificationData(
        id = "DISABLED_BY_DED",
        variables = List(DEDVariable(name = "emailId", typeInSrc = "ANY", lineNumber = None))
      )
    )
  )

}
