package ai.privado.languageEngine.python.passes.read

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.ScanProcessor
import ai.privado.languageEngine.java.semantic.SemanticGenerator
import ai.privado.languageEngine.java.tagger.sink.CustomInheritTagger
import ai.privado.languageEngine.python.feeder.StorageInheritRule
import ai.privado.languageEngine.python.tagger.source.IdentifierTagger
import ai.privado.model.{CatLevelOne, ConfigAndRules, Language, NodeType, RuleInfo}
import better.files.File
import io.joern.dataflowengineoss.layers.dataflows.{OssDataFlow, OssDataFlowOptions}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.joern.pysrc2cpg.{
  ImportsPass,
  Py2CpgOnFileSystem,
  Py2CpgOnFileSystemConfig,
  PythonNaiveCallLinker,
  PythonTypeHintCallLinker
}
import io.joern.dataflowengineoss.language._
import io.joern.x2cpg.X2Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{
  AnnotationParameterAssign,
  AstNode,
  CfgNode,
  JavaProperty,
  Literal,
  MethodParameterIn
}
import io.shiftleft.semanticcpg.language._

import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}

import java.nio.file.{Files, Paths}
import scala.collection.immutable.HashMap

class DjangoDBTest extends PythonDatabaseReadPassTest {
  override val modelFileContents =
    """from django.db import models
      |from django.contrib.auth.models import User
      |from ckeditor.fields import RichTextField
      |from crum import get_current_user
      |
      |
      |class User(models.Model):
      |    firstname = models.TextField('object du documnet', blank=True)
      |""".stripMargin

  override val viewFileContents =
    """
      |from .model import User
      |
      |user_obj = User.objects.get(pk=uid)
      |""".stripMargin

  "Database Read Pass" should {
    "identify method calls for read operations" in {
      val sources = Dataflow.getSources(cpg)
      val sinks   = Dataflow.getSinks(cpg)
      implicit val engineContext: EngineContext =
        EngineContext(semantics = SemanticGenerator.getSemantics(cpg, ScanProcessor.config), config = EngineConfig(4))

      println(sources)
      println(sinks)
      println(sinks.reachableByFlows(sources).l)

    }
  }

}

abstract class PythonDatabaseReadPassTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  var cpg: Cpg                  = _
  var inputDir: File            = _
  var outputFile: File          = _
  val modelFileContents: String = ""
  val viewFileContents: String  = ""

  override def beforeAll(): Unit = {
    inputDir = File.newTemporaryDirectory()

    (inputDir / "unrelated.file").write("foo")
    (inputDir / "model.py").write(modelFileContents)
    (inputDir / "view.py").write(viewFileContents)

    outputFile = File.newTemporaryFile()

    val taggerCache: TaggerCache = new TaggerCache

    val sourceRule = List(
      RuleInfo(
        "Data.Sensitive.FirstName",
        "FirstName",
        "",
        Array(),
        List("(?i).*firstname.*"),
        false,
        "",
        Map(),
        NodeType.REGULAR,
        "",
        CatLevelOne.SOURCES,
        "",
        Language.JAVA,
        Array()
      )
    )

    val sinkRules = List(
      RuleInfo(
        "Storages.Framework.DjangoDB.Write",
        "DjangoDB(Write)",
        "",
        Array[String]("djangoproject.com"),
        List[String]("(?i)django/db/models.py:<module>.Model", "(save|create)"),
        false,
        "",
        HashMap[String, String](),
        NodeType.REGULAR,
        "",
        CatLevelOne.SINKS,
        "storages",
        Language.PYTHON,
        Array[String]()
      ),
      RuleInfo(
        "Storages.Framework.DjangoDB.Read",
        "DjangoDB(Read)",
        "",
        Array[String]("djangoproject.com"),
        List[String]("(?i)django/db/models.py:<module>.Model", "(filter|get)"),
        false,
        "",
        HashMap[String, String](),
        NodeType.REGULAR,
        "",
        CatLevelOne.SINKS,
        "storages",
        Language.PYTHON,
        Array[String]()
      )
    )

    val pythonConfig = Py2CpgOnFileSystemConfig(Paths.get(outputFile.toString()), Paths.get(inputDir.toString()))
    cpg = new Py2CpgOnFileSystem().createCpg(pythonConfig).get

    // Apply default overlays
    X2Cpg.applyDefaultOverlays(cpg)
    new ImportsPass(cpg).createAndApply()
    new PythonTypeHintCallLinker(cpg).createAndApply()
    new PythonNaiveCallLinker(cpg).createAndApply()

    // Apply OSS Dataflow overlay
    new OssDataFlow(new OssDataFlowOptions()).run(new LayerCreatorContext(cpg))

    val rule: ConfigAndRules =
      ConfigAndRules(sourceRule, sinkRules, List(), List(), List(), List(), List(), List(), List())
    RuleCache.setRule(rule)

    new IdentifierTagger(cpg, taggerCache).createAndApply()
    sinkRules.foreach(RuleCache.setRuleInfo)
    new CustomInheritTagger(cpg).createAndApply()

    super.beforeAll()
  }

  override def afterAll(): Unit = {
    inputDir.delete()
    cpg.close()
    outputFile.delete()
    super.afterAll()
  }

}
