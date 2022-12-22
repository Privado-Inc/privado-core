package ai.privado.utility

import scala.util.matching.Regex
import scala.util.control.Breaks._
import java.net.MalformedURLException
import java.nio.charset.MalformedInputException


object ImportUtility {  

    private var allImports: Set[String] = Set();
    private def getMatchImportRegex(language: String): String = {
        val result = language match {
            case "java" => "^\\s*(import)\\s+.*$"
            case "py" => "^\\s*(import)\\s+.*$"
            case "js" => "^\\s*(import|require)\\s+.*$"
            case default => "(import)\\s+" // Default is java 
        }
        return result;
    }

    private def getLiteralWordRegex(language: String): String = {
        val result = language match {
            case "java" => "(import)\\s+" // Almost always is accompanied by a space at the end
            case "py" => "(import)\\s+" // Almost always is accompanied by a space at the end
            case default => "(import)\\s+" // Default is java 
        }
        return result;
    }

    private def returnImportStatements(filePath: String, language: String): Set[String] = {
        val source = io.Source.fromFile(filePath)
        val matchImportRegex = getMatchImportRegex(language);
        val onlyLiteralWordRegex = getLiteralWordRegex(language);

        var multilineFlag = false;
        var uniqueImports: Set[String] = Set()
        val result = new StringBuilder("")
        try {
            breakable
            {
                for (line <- source.getLines()) {
                        val scanLine = line.trim();
                        if (scanLine.matches("/\\*.*")) {
                            multilineFlag = true;
                        }    
                        if (!multilineFlag) {
                            if (scanLine matches matchImportRegex) {
                                val withoutImportLine = scanLine.replace(onlyLiteralWordRegex.r.findFirstIn(line).mkString, "");
                                uniqueImports += withoutImportLine;
                            } else {
                                if (!scanLine.matches("(package|//)\\s*.*") && scanLine.lengthCompare(0) != 0) {
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

    private def scanAllFilesInDirectory(dirpath: String): Unit = {
        val files = new java.io.File(dirpath).listFiles();
        for (file <- files) {
            if (file.isDirectory()) scanAllFilesInDirectory(file.getAbsolutePath())
            else {
                if (file.getAbsolutePath().matches(".+.(java)")) {
                    // println(file.getAbsolutePath())
                    var fileImports: Set[String] = returnImportStatements(file.getAbsolutePath(), "java");
                    for (el <- fileImports) {
                        allImports += el;
                    }
                }
            }
        }

    }

    def getAllImportsFromDirectory(dirpath: String): Set[String] = {
        scanAllFilesInDirectory(dirpath);
        return allImports;
    }

}  