package ai.privado.languageEngine.java.passes.config

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.cache.ModuleCache
import ai.privado.utility.Utilities
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, DiffGraphBuilder}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewModule, NewModuleDependency}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.apache.maven.model.Model
import org.apache.maven.model.io.xpp3.MavenXpp3Reader

import java.io.{File, FileReader}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Try
import better.files.File.VisitOptions

class ModuleFilePass(cpg: Cpg, projectRoot: String, moduleCache: ModuleCache, ruleCache: RuleCache)
    extends ForkJoinParallelCpgPass[String](cpg) {

  override def generateParts(): Array[String] =
    ModuleFiles(projectRoot, Set(".xml", ".gradle")).toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode              = addFileNode(file, builder)
    var moduleNode: NewModule = null
    val dependencyList        = mutable.Set[NewModuleDependency]()

    if (file.contains("pom.xml")) {
      val reader = new MavenXpp3Reader()
      val model  = reader.read(new FileReader(file))
      moduleNode = addMavenModuleNode(model, builder)
      val dependencies = model.getDependencies
      dependencies.forEach(dependency => {
        dependencyList += addDependencyNode(
          dependency.getGroupId,
          dependency.getArtifactId,
          dependency.getVersion,
          builder
        )
      })
      processMavenParentModule(model)
    } else {
      val buildGradleInfo = parseBuildGradle(file)
      moduleNode = addGradleModuleNode(buildGradleInfo.get, builder)
      buildGradleInfo.get.dependencies.foreach(dependency => {
        dependencyList += addDependencyNode(dependency.groupId, dependency.artifactId, dependency.version, builder)
      })
      processGradleParentModule(moduleNode.artifactid)
    }

    // Store module dependencies info in Map
    moduleCache.addDependenciesModule(moduleNode.artifactid, dependencyList)
    moduleCache.addModule(moduleNode.artifactid, moduleNode)

    builder.addEdge(moduleNode, fileNode, EdgeTypes.SOURCE_FILE)
    dependencyList.foreach(dependency => {
      builder.addEdge(dependency, fileNode, EdgeTypes.SOURCE_FILE)
      builder.addEdge(dependency, moduleNode, EdgeTypes.DEPENDENCY_MODULE)
    })
  }
  private def addGradleModuleNode(buildGradleInfo: BuildGradleInfo, builder: DiffGraphBuilder): NewModule = {
    val moduleNode = NewModule().groupid(buildGradleInfo.group).version(buildGradleInfo.version)
    builder.addNode(moduleNode)
    moduleNode
  }

  private def addMavenModuleNode(model: Model, builder: DiffGraphBuilder): NewModule = {
    val moduleNode = NewModule().groupid(model.getGroupId).artifactid(model.getArtifactId).version(model.getVersion)
    builder.addNode(moduleNode)
    moduleNode
  }

  private def ModuleFiles(projectRoot: String, extensions: Set[String]): List[String] = {
    val filePath = SourceFiles
      .determine(Set(projectRoot), extensions)(VisitOptions.default)
      .filter(Utilities.isFileProcessable(_, ruleCache))

    val moduleFile = ListBuffer[String]()
    filePath.foreach(path => {
      if (path.contains("pom") || path.contains("build")) {
        moduleFile += path
      }
    })
    moduleFile.toList
  }

  private def addFileNode(name: String, builder: DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }

  private def addDependencyNode(
    group: String,
    artifact: String,
    version: String,
    builder: DiffGraphBuilder
  ): NewModuleDependency = {
    val dependencyNode = NewModuleDependency().groupid(group).artifactid(artifact).version(version)
    builder.addNode(dependencyNode)
    dependencyNode
  }

  private case class DependencyParse(groupId: String, artifactId: String, version: String)

  private case class BuildGradleInfo(version: String, group: String, dependencies: Seq[DependencyParse])

  private def parseBuildGradle(path: String): Try[BuildGradleInfo] = Try {
    val file     = new File(path)
    val contents = Source.fromFile(file).mkString

    val versionRegex = """version\s*(=)\s*'(.+)'""".r
    val groupRegex   = """group\s*(=)\s*'(.+)'""".r
    val dependencyRegex =
      """(implementation|compileOnly|annotationProcessor|testImplementation)\s*'([^:]+):([^:]+):(.+)'""".r

    val version = versionRegex.findFirstMatchIn(contents).map(_.group(2)).getOrElse("")
    val group   = groupRegex.findFirstMatchIn(contents).map(_.group(2)).getOrElse("")
    val dependencies = dependencyRegex
      .findAllMatchIn(contents)
      .map { m =>
        DependencyParse(m.group(2), m.group(3), m.group(4))
      }
      .toList

    BuildGradleInfo(version, group, dependencies)
  }

  private def processMavenParentModule(model: Model): Unit = {
    val parentInfo = model.getParent
    if (parentInfo == null) {
      moduleCache.addSubModuleParent(model.getArtifactId, null)
    } else {
      moduleCache.addSubModuleParent(model.getArtifactId, parentInfo.getArtifactId)
    }
  }

  private def processGradleParentModule(childArtifactName: String): Unit = {
    moduleCache.addSubModuleParent(childArtifactName, null)
  }
}
