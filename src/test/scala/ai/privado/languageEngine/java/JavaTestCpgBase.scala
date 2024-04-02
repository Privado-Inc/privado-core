package ai.privado.languageEngine.java

import ai.privado.TestCpgBase
import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, RuleCache, S3DatabaseDetailsCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import ai.privado.languageEngine.java.processor.JavaProcessor
import ai.privado.model.Language
import ai.privado.rule.RuleInfoTestData
import io.circe.Json
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class JavaTestCpgBase(
  withRuleCache: RuleCache = RuleInfoTestData.ruleCache,
  withPrivadoInput: PrivadoInput = PrivadoInput()
) extends TestCpgBase(withPrivadoInput) {
  override def getProcessor(sourceCodeLocation: String): BaseProcessor = {
    appCache.init(sourceCodeLocation)
    appCache.repoLanguage = Language.JAVA
    new JavaProcessor(
      withRuleCache,
      withPrivadoInput.copy(sourceLocation = Set(sourceCodeLocation)),
      sourceCodeLocation,
      dataFlowCache,
      auditCache,
      s3DatabaseDetailsCache,
      appCache,
      returnClosedCpg = false
    )
  }

}
