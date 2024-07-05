/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

package ai.privado.cache
import ai.privado.model.Language
import ai.privado.model.Language.Language
import ai.privado.utility.Utilities.*

import java.util.regex.PatternSyntaxException
import scala.collection.mutable
import java.util.regex.*

/** Cache Used to store Application/Scan specific information
  */
class AppCache(
  var localScanPath: String = "",
  var scanPath: String = "",
  var repoName: String = "",
  var repoLanguage: Language = Language.UNKNOWN,
  var isLombokPresent: Boolean = false,
  var privadoVersionMain: String = "",
  var fpByOverlappingDE: Int = 0,
  var totalFlowFromReachableBy: Int = 0,
  var totalFlowAfterThisFiltering: Int = 0,
  var fpMap: mutable.HashMap[String, Int] = mutable.HashMap[String, Int](),
  var totalMap: mutable.HashMap[String, Int] = mutable.HashMap[String, Int](),
  var ingressUrls: mutable.ListBuffer[String] = mutable.ListBuffer[String](),
  var excludeFileRegex: Option[String] = None
) {

  def init(path: String, excludeFileRegex: String = ""): Unit = {
    this.scanPath = path.stripSuffix("/")      // Scan Path of the repo on the host machine
    this.localScanPath = getRepoScanPath(path) // scan path perceived by the binary (can be different inside docker)
    this.repoName = this.localScanPath.split("[/\\\\]").lastOption.getOrElse("")
    this.excludeFileRegex = isValidRegex(excludeFileRegex) match
      case true => Option(excludeFileRegex)
      case false =>
        None

  }

  /** Checks if the regex string is a valid one
    * @param regex
    * @return
    */
  private def isValidRegex(regex: String) = {
    try {
      Pattern.compile(regex)
      true
    } catch {
      case e: PatternSyntaxException =>
        println(s"The entered exclusion regex is invalid: $regex with exception : ${e.getMessage}")
        false
    }
  }
}
