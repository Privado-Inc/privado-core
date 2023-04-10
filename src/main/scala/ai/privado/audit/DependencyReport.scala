package ai.privado.audit

import ai.privado.languageEngine.java.language.module.{NodeStarters, StepsForModule}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.ModuleDependency
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object DependencyReport {

  private val logger = LoggerFactory.getLogger(getClass)

  def processDependencyAudit(xtocpg: Try[Cpg]): List[List[String]]  = {
    val workflowResult = new ListBuffer[List[String]]()
    val dependencies = getDependencyList(xtocpg)

    dependencies.foreach(dependency => {
      workflowResult += List(s"${dependency.groupid}-${dependency.artifactid}-${dependency.version}", dependency.file.head.name)
    })

    workflowResult.toList
  }

  def getDependencyList(xtocpg: Try[Cpg]): Set[ModuleDependency] = {
    val dependencies = new mutable.HashSet[ModuleDependency]()

    xtocpg match {
      case Success(cpg) => {
        val dependenciesSet = cpg.module.dependencies.l.toSet
        dependenciesSet.foreach(dependency => {
          dependencies += dependency
        })
      }
      case Failure(exception) => {
        println("Failed to process dependencies from cpg")
        logger.debug("Failed to process dependencies from cpg", exception)
        println(exception.printStackTrace())
      }
    }
    dependencies.toSet
  }
}
