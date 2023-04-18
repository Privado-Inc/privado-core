package ai.privado.utility

import ai.privado.cache.RuleCache

import scala.util.control.Breaks._
import java.nio.charset.MalformedInputException
import scala.io.Source
import ai.privado.model.Language

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

case class LanguageImportConfig(
  matchImportRegex: String,
  literalWordRegex: String,
  exclusionStatementRegex: String,
  multilineCommentStartRegex: String,
  multilineCommentEndRegex: String
);

object LanguageImportConfig {
  def apply(language: Language.Value): LanguageImportConfig = language match {
    case Language.JAVA =>
      new LanguageImportConfig(
        "^\\s*(import)\\s+.*$",
        "(import|static)\\s+",
        "^\\s*(package|//)\\s+.*$",
        "/\\*.*",
        ".*\\*/"
      )
    case Language.JAVASCRIPT =>
      new LanguageImportConfig(
        "^\\s*(import|require)\\s+.*$",
        "(import|require)\\s+",
        "^\\s*(//)\\s+.*$",
        "/\\*.*",
        "\".*\\\\*/\""
      )
  }
}

object ImportUtility {

  private def returnImportStatements(filePath: String, language: Language.Value): mutable.Set[String] = {
    val source                     = Source.fromFile(filePath)
    val languageImport             = LanguageImportConfig(language);
    val matchImportRegex           = languageImport.matchImportRegex;
    val onlyLiteralWordRegex       = languageImport.literalWordRegex;
    val exclusionStatementRegex    = languageImport.exclusionStatementRegex;
    val multilineCommentStartRegex = languageImport.multilineCommentStartRegex;
    val multilineCommentEndRegex   = languageImport.multilineCommentEndRegex;

    var multilineFlag                      = false;
    val uniqueImports: mutable.Set[String] = mutable.Set[String]()
    try {

      breakable {
        for (line <- source.getLines()) {
          val scanLine = line.trim();
          if (scanLine.matches(multilineCommentStartRegex)) { // set flag if multiline comment is encountered
            multilineFlag = true;
          }

          // Ignore if inside a multiline comment
          if (!multilineFlag) {
            if (scanLine matches matchImportRegex) {
              val withoutImportLine = scanLine.replace(onlyLiteralWordRegex.r.findAllIn(line).mkString, "");
              uniqueImports.add(withoutImportLine) // add each import to set
            } else {
              if (!scanLine.matches(exclusionStatementRegex) && scanLine.lengthCompare(0) != 0) { // Ignore if there is nothing or a package definition on a line
                break()
              }
            }
          }

          if (scanLine.matches(multilineCommentEndRegex)) {
            multilineFlag = false;
          }

        }
      }
    } catch {
      case e: MalformedInputException => println(e)
    } finally {
      source.close()
    }
    return uniqueImports;
  }

  def getAllImportsFromProject(dirpath: String, language: Language.Value, ruleCache: RuleCache): mutable.Set[String] = {
    val files = Utilities.getAllFilesRecursively(dirpath, Set("." + language.toString()), ruleCache).get
    val allImports: mutable.Set[String] = mutable.Set[String]()
    // .par used to convert list to a parallel operational list
    for (file <- files.par) {
      val fileImports: mutable.Set[String] = returnImportStatements(file, language);
      allImports.addAll(fileImports)
    }

    allImports;
  }

}
