package ai.privado.testfixtures

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, PropertyFilterCache, RuleCache, S3DatabaseDetailsCache}
import ai.privado.entrypoint.PrivadoInput
import io.circe.Json
import io.joern.x2cpg.utils.TestCodeWriter
import io.shiftleft.codepropertygraph.Cpg
import overflowdb.Graph

abstract class TestCpg extends Cpg() with TestCodeWriter with LanguageFrontend {
  private var _graph                                  = Option.empty[Graph]
  private var _privadoJson: Option[Map[String, Json]] = Option(Map())

  override def moreCode(code: String, fileName: String): TestCpg.this.type = {
    checkGraphEmpty()
    super.moreCode(code, fileName)
  }

  def withPrivadoInput(privadoInput: PrivadoInput): this.type = {
    setPrivadoInput(privadoInput)
    this
  }

  def withRuleCache(ruleCache: RuleCache): this.type = {
    setRuleCache(ruleCache)
    this
  }

  def withAuditCache(auditCache: AuditCache): this.type = {
    setAuditCache(auditCache)
    this
  }

  def withDataFlowCache(dataFlowCache: DataFlowCache): this.type = {
    setDataFlowCache(dataFlowCache)
    this
  }

  def withS3DatabaseDetailsCache(s3DatabaseDetailsCache: S3DatabaseDetailsCache): this.type = {
    setS3DatabaseDetailsCache(s3DatabaseDetailsCache)
    this
  }

  def withAppCache(appCache: AppCache): this.type = {
    setAppCache(appCache)
    this
  }

  def withPropertyFilterCache(propertyFilterCache: PropertyFilterCache): this.type = {
    setPropertyFilterCache(propertyFilterCache)
    this
  }

  def getPrivadoJson() = _privadoJson.get

  def generateScanResult(): this.type = {
    graph
    this
  }

  override def graph: Graph = {
    if (_graph.isEmpty) {
      val codeDir = writeCode(fileSuffix)
      try {
        val processor = getProcessor(codeDir.toFile)
        var cpg: Cpg  = io.shiftleft.codepropertygraph.generated.Cpg.empty
        processor.processCpg() match {
          case Left(error) => println(s"Error while creating cpg : $error")
          case Right(outputCpgWithJsonMap) =>
            cpg = outputCpgWithJsonMap.cpg
            _privadoJson = Option(outputCpgWithJsonMap.outputMap)
        }
        _graph = Option(cpg.graph)
      } finally {
        cleanupOutput()
      }
    }
    _graph.get
  }

  private def checkGraphEmpty(): Unit = {
    if (_graph.isDefined) {
      throw new RuntimeException("Modifying test data is not allowed after accessing graph.")
    }
  }

  override def close(): Unit = {
    _graph.foreach(_.close())
  }
}
