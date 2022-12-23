package ai.privado.utility

import scala.util.control.Breaks._
import java.nio.charset.MalformedInputException
import scala.io.Source
import ai.privado.model.Language

import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable

object ImportUtility {

  private var allImports: Set[String] = Set();

  // get language specific import matching regex
  private def getMatchImportRegex(language: Language.Value): String = {
    val result = language match {
      case Language.JAVA       => "^\\s*(import)\\s+.*$"
      case Language.PYTHON     => "^\\s*(import)\\s+.*$"
      case Language.JAVASCRIPT => "^\\s*(import|require)\\s+.*$"
      case default             => "(import)\\s+" // Default is java
    }
    return result;
  }

  private def getFileExtension(language: Language.Value): String = {
    val result = language match {
      case Language.JAVA       => ".java"
      case Language.PYTHON     => ".py"
      case Language.JAVASCRIPT => ".js"
      case default             => ".java" // Default is java
    }

    return result;
  }

  // get regex to match only import words
  private def getLiteralWordRegex(language: Language.Value): String = {
    val result = language match {
      case Language.JAVA       => "(import|static)\\s+"
      case Language.PYTHON     => "(import)\\s+"
      case Language.JAVASCRIPT => "(import|require)\\s+"
      case default             => "(import)\\s+" // Default is java
    }
    return result;
  }

  private def returnImportStatements(filePath: String, language: Language.Value): Set[String] = {
    val source               = Source.fromFile(filePath)
    val matchImportRegex     = getMatchImportRegex(language);
    val onlyLiteralWordRegex = getLiteralWordRegex(language);

    var multilineFlag              = false;
    var uniqueImports: Set[String] = Set()
    try {

      breakable {
        for (line <- source.getLines()) {
          val scanLine = line.trim();
          if (scanLine.matches("/\\*.*")) { // set flag if multiline comment is encountered
            multilineFlag = true;
          }

          // Ignore if inside a multiline comment
          if (!multilineFlag) {
            if (scanLine matches matchImportRegex) {
              val withoutImportLine = scanLine.replace(onlyLiteralWordRegex.r.findAllIn(line).mkString, "");
              uniqueImports += withoutImportLine; // add each import to set
            } else {
              if (!scanLine.matches("(package|//)\\s*.*") && scanLine.lengthCompare(0) != 0) { // Ignore if there is nothing or a package definition on a line
                break()
              }
            }
          }

          if (scanLine.matches(".*\\*/")) {
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

  private def scanAllFilesInDirectory(dirpath: String, language: Language.Value): Unit = {
    val files = Utilities.getAllFilesRecursively(dirpath, Set("." + language.toString())).get

    // .par used to convert list to a parallel operational list
    for (file <- files.par) {
      var fileImports: Set[String] = returnImportStatements(file, language);
      for (el <- fileImports) {
        allImports += el;
      }
    }
  }

  def getAllImportsFromDirectory(dirpath: String, language: Language.Value): Set[String] = {
    scanAllFilesInDirectory(dirpath, language);
    return allImports;
  }

}
