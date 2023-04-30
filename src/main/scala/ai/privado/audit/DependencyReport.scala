package ai.privado.audit

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.cache.DependencyModuleCache
import ai.privado.languageEngine.java.language.module.{NodeStarters, StepsForModule}
import ai.privado.languageEngine.java.passes.module.DependencyCategoryPass
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.ModuleDependency
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object DependencyReport {

  private val logger = LoggerFactory.getLogger(getClass)

  def processDependencyAudit(
    xtocpg: Try[Cpg],
    ruleCache: RuleCache,
    dependencies: Set[ModuleDependency]
  ): List[List[String]] = {
    val workbookResult = new ListBuffer[List[String]]()
    new DependencyCategoryPass(xtocpg.get, ruleCache, dependencies.toList).createAndApply()
    dependencies.foreach(dependency => {
      val category = getDependencyCategory(dependency)
      workbookResult += List(
        dependency.file.head.name,
        s"${dependency.groupid}.${dependency.artifactid}",
        dependency.artifactid,
        isDependencyProcessed(category),
        category,
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
      AuditReportConstants.DEPENDENCY_INTERNAL_LIBRARY_NAME
    } else {
      DependencyModuleCache.checkIfDependencyRuleExist(moduleDependency)
    }
  }

  private def checkIfDependencyIsInternalLibrary(moduleDependency: ModuleDependency): Boolean = {
    val moduleGroupId     = moduleDependency.dependencyModuleOut.head.groupid
    val dependencyGroupId = moduleDependency.groupid

    if (moduleGroupId == null || dependencyGroupId == null) false else dependencyGroupId.contains(moduleGroupId)
  }

  private def isDependencyProcessed(category: String): String = {
    category match {
      case AuditReportConstants.DEPENDENCY_INTERNAL_LIBRARY_NAME         => AuditReportConstants.AUDIT_NOT_CHECKED_VALUE
      case AuditReportConstants.DEPENDENCY_KNOW_THIRD_PARTY_LIBRARY_NAME => AuditReportConstants.AUDIT_CHECKED_VALUE
      case AuditReportConstants.DEPENDENCY_STORAGE_LIBRARY_NAME          => AuditReportConstants.AUDIT_CHECKED_VALUE
      case AuditReportConstants.DEPENDENCY_COLLECTION_LIBRARY_NAME       => AuditReportConstants.AUDIT_CHECKED_VALUE
      case AuditReportConstants.DEPENDENCY_WEB_CLIENT_LIBRARY_NAME       => AuditReportConstants.AUDIT_CHECKED_VALUE
      case AuditReportConstants.DEPENDENCY_UTILITY_LIBRARY_NAME          => AuditReportConstants.AUDIT_NOT_CHECKED_VALUE
      case AuditReportConstants.DEPENDENCY_UNKNOWN_LIBRARY_NAME          => AuditReportConstants.AUDIT_NOT_CHECKED_VALUE
      case _                                                             => AuditReportConstants.AUDIT_NOT_CHECKED_VALUE
    }
  }
}
