package ai.privado.languageEngine.java

import ai.privado.cache.RuleCache
import ai.privado.model.*
import ai.privado.model.Language.{JAVA, KOTLIN, Language}
import better.files.File
import io.joern.javasrc2cpg.JavaSrc2Cpg
import io.joern.kotlin2cpg.Kotlin2Cpg
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}

case class TestCodeSnippet(sourceCode: String, language: Language)

abstract class AbstractTaggingSpec extends AnyWordSpec with Matchers with BeforeAndAfterAll with BeforeAndAfterEach {

  override def beforeEach(): Unit = {
    super.beforeEach()
  }

  def buildCpg(codeSnippet: TestCodeSnippet): Cpg = {
    val inputDir = File.newTemporaryDirectory()
    inputDir.deleteOnExit()
    val testId = java.util.UUID.randomUUID.toString
    // create test directory
    val testDir = File.newTemporaryDirectory(testId, Some(inputDir))
    File
      .newTemporaryFile("sourceFile", LanguageFileExt.withLanguage(codeSnippet.language), Some(testDir))
      .writeText(codeSnippet.sourceCode)
    val outputFile = File.newTemporaryFile()
    var cpg: Cpg   = null
    if (codeSnippet.language == JAVA) {
      val config =
        io.joern.javasrc2cpg.Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
      cpg = new JavaSrc2Cpg().createCpg(config).get
    } else if (codeSnippet.language == KOTLIN) {
      val config =
        io.joern.kotlin2cpg.Config().withInputPath(inputDir.pathAsString).withOutputPath(outputFile.pathAsString)
      cpg = new Kotlin2Cpg().createCpg(config).get
    } else {
      throw RuntimeException("Unknown language encountered while building test CPG")
    }
    applyDefaultOverlays(cpg)
    cpg
  }

  def ruleCacheWithSourceAndCollectionRules(sourceRules: List[RuleInfo], collectionRules: List[RuleInfo]): RuleCache = {
    val ruleCache = RuleCache()
    ruleCache.setRule(ConfigAndRules(sources = sourceRules, collections = collectionRules))
    ruleCache
  }

}
