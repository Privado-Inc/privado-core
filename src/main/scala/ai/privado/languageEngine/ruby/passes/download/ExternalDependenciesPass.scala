package ai.privado.languageEngine.ruby.passes.download

import ai.privado.languageEngine.ruby.download.JRubyBasedParser
import better.files.File
import io.joern.rubysrc2cpg.utils.PackageTable
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.jruby.Ruby
import org.jruby.ast.{Colon2Node, DefnNode, Node, NodeType, ReturnNode}
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala

class ExternalDependenciesPass(cpg: Cpg, tempExtDir: String, packageTable: PackageTable, inputPath: String)
    extends ConcurrentWriterCpgPass[String](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[String] = getRubyDependenciesFile(tempExtDir)

  override def runOnPart(diffGraph: DiffGraphBuilder, filePath: String): Unit = {
    val moduleName = resolveModuleNameFromPath(filePath)
    try {
      processRubyDependencyFile(filePath, moduleName)
    } catch {
      case ex: Exception =>
        println(s"Error while parsing $moduleName module File ($filePath): ${ex.getMessage}")
    }
  }

  private def fetchMethodInfoFromNode(node: Node, currentNameSpace: List[String], moduleName: String): Unit = {
    try {
      if (node != null) {
        node.getNodeType match {
          case NodeType.CLASSNODE | NodeType.MODULENODE =>
            val childList = node.childNodes().asScala.toList
            if (childList.nonEmpty) {
              val classOrModuleName = childList.head.asInstanceOf[Colon2Node].getName.toString
              val classPath         = currentNameSpace.mkString(".")
              if (node.getNodeType == NodeType.CLASSNODE)
                packageTable.addTypeDecl(
                  moduleName,
                  classOrModuleName,
                  s"$classPath.$classOrModuleName".stripPrefix(".")
                )
              else
                packageTable.addModule(moduleName, classOrModuleName, s"$classPath.$classOrModuleName".stripPrefix("."))
              node
                .childNodes()
                .forEach(childNode =>
                  fetchMethodInfoFromNode(childNode, currentNameSpace ++ Seq(classOrModuleName), moduleName)
                )
            }
          case NodeType.DEFNNODE =>
            val methodName = node.asInstanceOf[DefnNode].getName.toString
            val classPath  = currentNameSpace.mkString(".")
            packageTable.addPackageMethod(moduleName, methodName, classPath, "<extMod>")
          case _ =>
            node.childNodes().forEach(childNode => fetchMethodInfoFromNode(childNode, currentNameSpace, moduleName))
        }
      }
    } catch {
      case e: Exception =>
        logger.info(s"Error when fetching methodInfo from node $moduleName, ${e.getMessage}")
        logger.debug(s"Error when fetching methodInfo from node $moduleName, ", e)
    }

  }

  private def processRubyDependencyFile(inputPath: String, moduleName: String): Unit = {
    try {
      if (File(inputPath).exists) {
        val rootNode = JRubyBasedParser.parseFile(inputPath)
        fetchMethodInfoFromNode(rootNode, List.empty, moduleName)
      }
    } catch {
      case e: Exception =>
        logger.info(s"Error while processing ruby dependency file $moduleName, ${e.getMessage}")
        logger.debug(s"Error while processing ruby dependency file $moduleName, ", e)
    }
  }

  private def getRubyDependenciesFile(inputPath: String): Array[String] = {
    val currentDir = File(inputPath)
    if (currentDir.exists) {
      currentDir.listRecursively.filter(_.extension.exists(_ == ".rb")).map(_.path.toString).toArray
    } else {
      Array.empty
    }
  }

  private def resolveModuleNameFromPath(path: String): String = {
    try {
      if (path.contains(tempExtDir)) {
        val moduleNameRegex = Seq("unpack", "([^", "]+)", "lib", ".*").mkString(java.io.File.separator).r
        moduleNameRegex
          .findFirstMatchIn(path)
          .map(_.group(1))
          .getOrElse("")
          .split(java.io.File.separator)
          .last
          .split("-")
          .head
      } else {
        path
      }
    } catch {
      case ex: Exception =>
        logger.info(s"Error while fetching module name from $path path: ${ex.getMessage}")
        "Unknown"
    }
  }
}
