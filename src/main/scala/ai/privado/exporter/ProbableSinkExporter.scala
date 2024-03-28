package ai.privado.exporter

import ai.privado.cache.{AppCache, RuleCache}
import ai.privado.model.{CatLevelOne, Constants, Language}
import ai.privado.utility.Utilities.{getAllFilesRecursively, getAllFilesRecursivelyWithoutExtension, isPrivacySink}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import scala.util.Using
import io.circe.parser.{parse, *}
import io.circe.*

class ProbableSinkExporter(cpg: Cpg, ruleCache: RuleCache, repoPath: String, repoItemTagName: Option[String] = None) {
  private val logger = LoggerFactory.getLogger(getClass)

  def getProbableSinks: List[String] = {

    val lang         = AppCache.repoLanguage
    val isPython     = lang.toString().contains(Language.PYTHON.toString)
    val isJavascript = lang.toString().contains(Language.JAVASCRIPT.toString)
    val isRuby       = lang.toString().contains(Language.RUBY.toString)
    val isGoLang     = lang.toString().contains(Language.GO.toString)
    val isPHP        = lang.toString().contains(Language.PHP.toString)

    if (repoItemTagName.isDefined)
      List() // If this is an export for Monolith repoItem, don't export Probable sink, otherwise this will make the Json very big and will need separate processing on backend
    else if (isJavascript) {
      getProbableSinkForJavascript(repoPath)
    } else if (isRuby) {
      getProbableSinkForRuby(repoPath)
    } else if (isPHP) {
      val composerDep = getProbableSinkForPHP(repoPath)
      composerDep ++ getProbableSinkBasedOnTaggedMethods(isPython, isGoLang)
    } else {
      getProbableSinkBasedOnTaggedMethods(isPython, isGoLang)
    }
  }

  def getProbableSinkForJavascript(repoPath: String): List[String] = {
    // Set up a set to hold the unique dependencies
    var uniqueDeps = Set.empty[String]
    val packageJsonFilePaths =
      getAllFilesRecursively(repoPath, Set(".json"), ruleCache)
        .getOrElse(List.empty)
        .filter(_.endsWith("package.json"))

    for (path <- packageJsonFilePaths) {
      Using(scala.io.Source.fromFile(path)) { source =>
        val packageJsonStr = source.mkString
        val json           = parse(packageJsonStr).getOrElse(Json.Null)
        val dependencies   = json.hcursor.downField("dependencies").as[Map[String, String]].getOrElse(Map.empty)
        uniqueDeps ++= dependencies.keySet
      }
    }
    uniqueDeps.toList
      .filter((str) => isPrivacySink(str, ruleCache))
  }

  def getProbableSinkForPHP(repoPath: String): List[String] = {
    // Set up a set to hold the unique dependencies
    var uniqueDeps = Set.empty[String]
    val packageJsonFilePaths =
      getAllFilesRecursively(repoPath, Set(".json"), ruleCache)
        .getOrElse(List.empty)
        .filter(_.endsWith("composer.json"))

    for (path <- packageJsonFilePaths) {
      Using(scala.io.Source.fromFile(path)) { source =>
        val packageJsonStr = source.mkString
        val json           = parse(packageJsonStr).getOrElse(Json.Null)
        val dependencies   = json.hcursor.downField("require").as[Map[String, String]].getOrElse(Map.empty)
        uniqueDeps ++= dependencies.keySet
      }
    }
    uniqueDeps.toList
      .filter((str) => isPrivacySink(str, ruleCache))
  }

  def getProbableSinkForRuby(repoPath: String): List[String] = {
    // Set up a set to hold the unique dependencies
    val gemFilePaths =
      getAllFilesRecursivelyWithoutExtension(repoPath, "Gemfile")
        .getOrElse(List.empty)
        .filter(_.endsWith("Gemfile"))
    val uniqueDependencies = gemFilePaths.flatMap(parseGemfileDependencies).toSet.toList
    uniqueDependencies.filter(isPrivacySink(_, ruleCache))
  }

  def parseGemfileDependencies(filePath: String): List[String] = {
    val gemfileLines = scala.io.Source.fromFile(filePath).getLines().toList
    val gemLines     = gemfileLines.filter(_.startsWith("gem"))

    gemLines.flatMap { line =>
      val gemName = line.stripPrefix("gem").trim().split(",").headOption.map(_.trim())
      gemName.map(_.stripPrefix("'").stripSuffix("'").stripPrefix("\"").stripSuffix("\""))
    }
  }

  def getProbableSinkBasedOnTaggedMethods(isPython: Boolean, isGoLang: Boolean): List[String] = {

    /** Get all the Methods which are tagged as SINKs */
    val taggedSinkMethods = cpg.call
      .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SINKS.name))
      .l
      .map(i => {
        var res = i.methodFullName
        if (!isPython) {
          res = res.split(":").headOption.getOrElse("")
        } else if (isGoLang) {
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
      } else if (!isGoLang) {
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
      .filter((str) => !str.matches(".*<operator>.*"))
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

}
