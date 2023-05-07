package ai.privado.exporter

import ai.privado.cache.RuleCache
import ai.privado.metric.MetricHandler
import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.utility.Utilities.isPrivacySink
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import org.json4s._
import org.json4s.native.JsonMethods._

import java.io.File

class ProbableSinkExporter(cpg: Cpg, ruleCache: RuleCache, repoPath: String) {
  private val logger = LoggerFactory.getLogger(getClass)

  def getProbableSinks: List[String] = {

    val lang         = MetricHandler.metricsData("language")
    val isPython     = lang.toString().contains(Languages.PYTHONSRC)
    val isJavascript = lang.toString().contains(Languages.JSSRC)

    if (isJavascript) {
      getProbableSinkForJavascript(repoPath)
    } else {
      getProbableSinkBasedOnTaggedMethods(isPython)
    }
  }

  def getProbableSinkForJavascript(repoPath: String): List[String] = {
    // Set up a set to hold the unique dependencies
    var uniqueDeps       = Set.empty[String]
    val file             = new java.io.File(repoPath)
    val packageJsonPaths = findAllPackageJsonFiles(file)

    for (path <- packageJsonPaths) {
      val packageJsonStr   = scala.io.Source.fromFile(path).mkString
      val packageJson      = parse(packageJsonStr)
      implicit val formats = DefaultFormats
      val dependencies = (packageJson \ "dependencies")
        .extractOpt[Map[String, String]]
        .getOrElse(Map.empty)
      uniqueDeps ++= dependencies.keySet
    }
    println(uniqueDeps)
    uniqueDeps.toList
      .filter((str) => isPrivacySink(str, ruleCache))
  }

  def getProbableSinkBasedOnTaggedMethods(isPython: Boolean): List[String] = {

    /** Get all the Methods which are tagged as SINKs */
    val taggedSinkMethods = cpg.tag
      .where(_.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
      .call
      .l
      .map(i => {
        var res = i.methodFullName
        if (!isPython) {
          res = res.split(":").headOption.getOrElse("")
        }
        res
      })
      .distinct

    logger.debug("Tagged Sink Methods: ", taggedSinkMethods.length)

    /** Get all the Methods which are external */
    val dependenciesTPs = cpg.method.external.l.map(i => {
      var res = i.fullName
      if (!isPython) {
        res = res.split(":").headOption.getOrElse("")
      }
      res
    })

    logger.debug("Dependencies TPS: ", dependenciesTPs.length)
    logger.debug("Total Methods: ", cpg.method.l.length)

    /** Actions: by excluding taggedSinkMethods check isPrivacySink transform method FullName close to groupIds remove
      * duplicates
      */
    val filteredTPs = dependenciesTPs
      .filter(str => !taggedSinkMethods.contains(str))
      .filter((str) => isPrivacySink(str, ruleCache))
      .filter((str) => !str.endsWith(".println"))
      .map((str) => {
        try {
          str.split("\\.").take(6).mkString(".").split(":").headOption.getOrElse("")
        } catch {
          case _: Exception => str
        }
      })
      .distinct

    logger.debug("Filtered TPS: ", filteredTPs)
    logger.debug("Filtered TPs Count: ", filteredTPs.length)
    filteredTPs
  }

  def findAllPackageJsonFiles(dir: File): List[File] = {
    if (dir.isDirectory) {
      dir.listFiles
        .filter(_.isFile)
        .filter(_.getName == "package.json")
        .toList ++ dir.listFiles
        .filter(_.isDirectory)
        .filter(_.getName != "node_modules")
        .flatMap(findAllPackageJsonFiles)
    } else {
      List.empty
    }
  }

}
