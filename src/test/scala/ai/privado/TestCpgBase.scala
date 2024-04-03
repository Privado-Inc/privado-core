package ai.privado

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, S3DatabaseDetailsCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import io.circe.Json
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class TestCpgBase(privadoInput: PrivadoInput) extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  val auditCache: AuditCache = new AuditCache()

  val dataFlowCache: DataFlowCache = new DataFlowCache(privadoInput, auditCache)

  val s3DatabaseDetailsCache: S3DatabaseDetailsCache = new S3DatabaseDetailsCache

  val appCache: AppCache = new AppCache()

  def getProcessor(sourceCodeLocation: String): BaseProcessor = ???

  def withCpg(sourceCodeLocation: String): Cpg = withCpgAndJson(sourceCodeLocation)._1

  def withJson(sourceCodeLocation: String): Map[String, Json] = withCpgAndJson(sourceCodeLocation)._2

  def withCpgAndJson(sourceCodeLocation: String): (Cpg, Map[String, Json]) = {
    val processor: BaseProcessor       = getProcessor(sourceCodeLocation)
    var cpg: Cpg                       = Cpg.empty
    var privadoJson: Map[String, Json] = Map()
    processor.processCpg() match
      case Left(error) => println(s"Error while creating cpg : $error")
      case Right(outputCpgWithJsonMap) =>
        cpg = outputCpgWithJsonMap.cpg
        privadoJson = outputCpgWithJsonMap.outputMap
    (cpg, privadoJson)
  }

}
