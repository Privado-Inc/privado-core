package ai.privado.utility
import scala.util.matching.Regex
import scala.util.control.Breaks._
import java.net.MalformedURLException
import java.nio.charset.MalformedInputException
import scala.io.Source

object ImportUtility {

  private var allImports: Set[String] = Set();

  // get language specific import matching regex
  private def getMatchImportRegex(language: String): String = {
    val result = language match {
      case "java"  => "^\\s*(import)\\s+.*$"
      case "py"    => "^\\s*(import)\\s+.*$"
      case "js"    => "^\\s*(import|require)\\s+.*$"
      case default => "(import)\\s+" // Default is java
    }
    return result;
  }

  private def getFileExtension(language: String): String = {
    val result = language match {
      case "java"       => ".java"
      case "python"     => ".py"
      case "javascript" => ".js"
      case default      => ".java" // Default is java
    }

    return result;
  }

  // get regex to match only import words
  private def getLiteralWordRegex(language: String): String = {
    val result = language match {
      case "java"  => "(import)\\s+" // Almost always is accompanied by a space at the end
      case "py"    => "(import)\\s+" // Almost always is accompanied by a space at the end
      case default => "(import)\\s+" // Default is java
    }
    return result;
  }

  private def returnImportStatements(filePath: String, language: String): Set[String] = {
    val source               = Source.fromFile(filePath)
    val matchImportRegex     = getMatchImportRegex(language);
    val onlyLiteralWordRegex = getLiteralWordRegex(language);

    var multilineFlag              = false;
    var uniqueImports: Set[String] = Set()
    val result                     = new StringBuilder("")
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
              val withoutImportLine = scanLine.replace(onlyLiteralWordRegex.r.findFirstIn(line).mkString, "");
              uniqueImports += withoutImportLine; // add each import to set
            } else {
              if (!scanLine.matches("(package|//)\\s*.*") && scanLine.lengthCompare(0) != 0) { // Ignore if there is nothing or a package definition on a line
                break
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
    }

    source.close()
    return uniqueImports;
  }

  private def scanAllFilesInDirectory(dirpath: String, language: String): Unit = {
    val files         = new java.io.File(dirpath).listFiles();
    val fileExtension = getFileExtension(language);

    for (file <- files) {
      if (file.isDirectory())
        scanAllFilesInDirectory(file.getAbsolutePath(), language) // Recursively call for each directory
      else {
        if (file.getAbsolutePath().endsWith(fileExtension)) { // Scan only java files
          var fileImports: Set[String] = returnImportStatements(file.getAbsolutePath(), language);
          for (el <- fileImports) {
            allImports += el;
          }
        }
      }
    }

  }

  def getAllImportsFromDirectory(dirpath: String, language: String): Set[String] = {
    scanAllFilesInDirectory(dirpath, language);
    return allImports;
  }
}
