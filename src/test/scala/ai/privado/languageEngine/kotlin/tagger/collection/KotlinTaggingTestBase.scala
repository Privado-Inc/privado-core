package ai.privado.languageEngine.kotlin.tagger.collection

import ai.privado.cache.RuleCache
import better.files.File
import io.joern.kotlin2cpg.{Config, Kotlin2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

abstract class KotlinTaggingTestBase extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg         = _
  var inputDir: File   = _
  var outputFile: File = _

  def initCpg(fileContents: String): Unit = {
    inputDir = File.newTemporaryDirectory()
    (inputDir / "generalFile.kt").write(fileContents)
    (inputDir / "unrelated.file").write("foo")
    outputFile = File.newTemporaryFile()
    val config = Config()
      .withInputPath(inputDir.pathAsString)
      .withOutputPath(outputFile.pathAsString)
    cpg = new Kotlin2Cpg().createCpg(config).get
  }

  def getRuleCache: RuleCache

  override def beforeAll(): Unit = {
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    outputFile.delete()
    cpg.close()
    super.afterAll()
  }

}
