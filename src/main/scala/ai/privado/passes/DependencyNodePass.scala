package ai.privado.passes

import ai.privado.inputprocessor.DependencyInfo
import ai.privado.tagger.PrivadoSimpleCpgPass
import ai.privado.utility.Utilities
import io.shiftleft.codepropertygraph.generated.nodes.{NewDependency, NewFile}
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}

import scala.collection.mutable

/** This pass is using single threaded CPG pass mechanism.
  *   a. The contents to be processed are not going to be very huge as the list contains only the identified 3p
  *      dependencies only. b. We need to create a file node. Which can be repeated across the multiple nodes. If we use
  *      parallel mechanism. It will be difficult to handle that use case.
  *
  * @param cpg
  * @param dependencies
  * @param projectRoot
  */
class DependencyNodePass(cpg: Cpg, dependencies: List[DependencyInfo], projectRoot: String)
    extends PrivadoSimpleCpgPass(cpg)
    with Utility {
  val fileNodeMap: mutable.Map[String, NewFile] = mutable.Map[String, NewFile]()
  def run(builder: DiffGraphBuilder): Unit = {
    dependencies.foreach(dependency => {
      val dep = NewDependency()
        .name(dependency.getFullDependencyName())
        .version(dependency.version)
        .lineNumber(dependency.lineNumber)
        .code(dependency.code)

      val fileNode = fileNodeMap.get(dependency.filePath) match {
        case Some(fileNode) => fileNode
        case None =>
          val fileNode = Utilities.addFileNode(dependency.filePath, builder)
          fileNodeMap.put(dependency.filePath, fileNode)
          fileNode
      }
      builder.addNode(dep)
      builder.addEdge(dep, fileNode, EdgeTypes.SOURCE_FILE)
    })
  }
}
