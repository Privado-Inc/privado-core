package ai.privado.languageEngine.java.passes.config

import ai.privado.utility.Utilities
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFile, NewJavaConfiguration, NewJavaDependency}
import io.shiftleft.passes.ForkJoinParallelCpgPass
import overflowdb.BatchedUpdate
import org.apache.maven.model.Model
import org.apache.maven.model.io.xpp3.MavenXpp3Reader

import java.io.{File, FileReader}
import scala.collection.mutable.ListBuffer

class ConfigurationFilePass(cpg: Cpg, projectRoot: String) extends ForkJoinParallelCpgPass[String](cpg){
  override def generateParts(): Array[String] =
    configurationFiles(projectRoot, Set(".xml", ".build")).toArray

  override def runOnPart(builder: DiffGraphBuilder, file: String): Unit = {
    val fileNode = addFileNode(file, builder)
    val pomModel = getConfigurationModel(file)
    val configurationNode = addConfigurationNode(pomModel, builder)
    val dependencyList = pomModel.getDependencies
    builder.addEdge(configurationNode, fileNode, EdgeTypes.SOURCE_FILE)
    dependencyList.forEach(dependency => {
      builder.addEdge(configurationNode, addDependencyNode(dependency.getGroupId, dependency.getArtifactId, dependency.getVersion, builder), EdgeTypes.DEPENDENCIES)
    })
  }

  private def addConfigurationNode(model: Model, builder: DiffGraphBuilder): NewJavaConfiguration = {
    val configurationNode = NewJavaConfiguration().groupid(model.getGroupId).artifactid(model.getArtifactId).version(model.getVersion)
    builder.addNode(configurationNode)
    configurationNode
  }

  private def getConfigurationModel(file: String): Model = {
    val reader = new MavenXpp3Reader()
    println(file)
    reader.read(new FileReader(file))
  }

  private def configurationFiles(projectRoot: String, extensions: Set[String]): List[String] = {
    val filePath = SourceFiles
      .determine(Set(projectRoot), extensions)
      .filter(Utilities.isFileProcessable)

    val configFile = ListBuffer[String]()
    filePath.foreach(path => {
      if (path.contains("pom") || path.contains("gradle")) {
        configFile += path
      }
    })
    configFile.toList
  }

  private def addFileNode(name: String, builder: BatchedUpdate.DiffGraphBuilder): NewFile = {
    val fileNode = NewFile().name(name)
    builder.addNode(fileNode)
    fileNode
  }

  private def addDependencyNode(group: String, artifact: String, version: String, builder: DiffGraphBuilder): NewJavaDependency = {
    val dependencyNode = NewJavaDependency().groupid(group).artifactid(artifact).version(version)
    builder.addNode(dependencyNode)
    dependencyNode
  }
}
