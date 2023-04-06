package ai.privado.languageEngine.java.passes.config

import ai.privado.utility.Utilities
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewModule, NewModuleDependency}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import overflowdb.BatchedUpdate
import org.apache.maven.model.Model
import org.apache.maven.model.io.xpp3.MavenXpp3Reader

import java.io.{File, FileReader}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Try

class ModuleFilePass(cpg: Cpg, projectRoot: String) extends ForkJoinParallelCpgPass[String](cpg) {

  private val dependenciesModuleMap = new mutable.HashMap[String, List[NewModuleDependency]]()

  private val subModuleParentMap = new mutable.HashMap[String, String]()

  override def generateParts(): Array[String] =
    ModuleFiles(projectRoot, Set(".xml", ".gradle")).toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode              = addFileNode(file, builder)
    var moduleNode: NewModule = null
    val dependencyList        = ListBuffer[NewModuleDependency]()

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
      ProcessMavenParentModule(model, subModuleParentMap)
    } else {
      val buildGradleInfo = parseBuildGradle(file)
      moduleNode = addGradleModuleNode(buildGradleInfo.get, builder)
      buildGradleInfo.get.dependencies.foreach(dependency => {
        dependencyList += addDependencyNode(dependency.groupId, dependency.artifactId, dependency.version, builder)
      })
    }

    // Store module dependencies info in Map
    dependenciesModuleMap.put(s"${moduleNode.groupid}-${moduleNode.artifactid.getOrElse("")}", dependencyList.toList)

    builder.addEdge(moduleNode, fileNode, EdgeTypes.SOURCE_FILE)
    dependencyList.foreach(dependency => {
      builder.addEdge(moduleNode, dependency, EdgeTypes.DEPENDENCIES)
      builder.addEdge(dependency, fileNode, EdgeTypes.SOURCE_FILE)
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
      .determine(Set(projectRoot), extensions)
      .filter(Utilities.isFileProcessable)

    val moduleFile = ListBuffer[String]()
    filePath.foreach(path => {
      if (path.contains("pom") || path.contains("build")) {
        moduleFile += path
      }
    })
    moduleFile.toList
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
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

  private def ProcessMavenParentModule(model: Model, subModuleParentMap: mutable.HashMap[String, String]): Unit = {
    val parentInfo = model.getParent
    if (parentInfo == null) {
      subModuleParentMap.put(s"${model.getGroupId}-${model.getArtifactId}", null)
    } else {
      subModuleParentMap.put(s"${model.getGroupId}-${model.getArtifactId}", s"${parentInfo.getGroupId}-${parentInfo.getArtifactId}")
    }
  }
}
