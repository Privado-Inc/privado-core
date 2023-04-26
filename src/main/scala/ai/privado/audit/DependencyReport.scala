package ai.privado.audit

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.cache.DependencyModuleCache
import ai.privado.languageEngine.java.language.module.{NodeStarters, StepsForDependency, StepsForModule}
import ai.privado.languageEngine.java.passes.module.DependencyCategoryPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.edges.DependencyModule
import io.shiftleft.codepropertygraph.generated.nodes.ModuleDependency
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object DependencyReport {

  private val logger = LoggerFactory.getLogger(getClass)

  def processDependencyAudit(xtocpg: Try[Cpg], ruleCache: RuleCache): List[List[String]] = {
    val workbookResult = new ListBuffer[List[String]]()
    val dependencies   = getDependencyList(xtocpg)
    new DependencyCategoryPass(xtocpg.get, ruleCache, dependencies.toList).createAndApply()

    dependencies.foreach(dependency => {
      workbookResult += List(
        dependency.file.head.name,
        s"${dependency.groupid}.${dependency.artifactid}",
        dependency.artifactid,
        AuditReportConstants.AUDIT_EMPTY_CELL_VALUE,
        getDependencyCategory(dependency),
        DependencyModuleCache.getDependencyRuleIfExist(dependency)
      )
    })

    List(
      List(
        AuditReportConstants.DEPENDENCY_FILE_PATH_NAME,
        AuditReportConstants.DEPENDENCY_LIBRARY_NAME,
        AuditReportConstants.DEPENDENCY_ARTIFACT_NAME,
        AuditReportConstants.DEPENDENCY_PROCESSED_NAME,
        AuditReportConstants.DEPENDENCY_CATEGORY_NAME,
        AuditReportConstants.DEPENDENCY_MATCHING_RULE_NAME
      )
    ) ++ workbookResult.groupBy(_.head).values.flatMap(identity).toList
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

  private def getDependencyCategory(moduleDependency: ModuleDependency): String = {
    if (checkIfDependencyIsInternalLibrary(moduleDependency)) {
      "INTERNAL LIBRARY"
    } else if (DependencyModuleCache.checkIfDependencyRuleExist(moduleDependency) == "third_parties") {
      "KNOWN THIRD PARTY"
    } else if (DependencyModuleCache.checkIfDependencyRuleExist(moduleDependency) == "storages") {
      "STORAGES"
    } else if (DependencyModuleCache.checkIfDependencyRuleExist(moduleDependency) == "leakages") {
      "LEAKAGES"
    } else {
      "UNKNOWN LIBRARY"
    }
  }

  private def checkIfDependencyIsInternalLibrary(moduleDependency: ModuleDependency): Boolean = {
    val moduleGroupId = moduleDependency.dependencyModuleOut.head.groupid
    val dependencyGroupId = moduleDependency.groupid

    if (moduleGroupId == null || dependencyGroupId == null) false else dependencyGroupId.contains(moduleGroupId)
  }
}
