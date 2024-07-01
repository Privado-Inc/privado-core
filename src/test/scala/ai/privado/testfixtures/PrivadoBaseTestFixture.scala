package ai.privado.testfixtures

import ai.privado.cache.{AppCache, AuditCache, DataFlowCache, PropertyFilterCache, RuleCache, S3DatabaseDetailsCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.languageEngine.base.processor.BaseProcessor
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, Inside}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable

class PrivadoBaseTestFixture[T <: TestCpg](testCpgFactory: () => T)
    extends AnyWordSpec
    with Matchers
    with BeforeAndAfterAll
    with BeforeAndAfterEach
    with Inside {

  private val cpgs = mutable.ArrayBuffer.empty[TestCpg]

  def code(code: String): T = {
    val newCpg = testCpgFactory().moreCode(code)
    cpgs.append(newCpg)
    newCpg
  }

  def code(code: String, fileName: String): T = {
    val newCpg = testCpgFactory().moreCode(code, fileName)
    cpgs.append(newCpg)
    newCpg
  }

  override def afterAll(): Unit = {
    cpgs.foreach(_.close())
  }
}
