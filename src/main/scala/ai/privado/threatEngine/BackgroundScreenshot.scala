/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

package ai.privado.threatEngine

import ai.privado.model.{CatLevelOne, Constants}
import ai.privado.utility.Utilities
import better.files.File
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import ai.privado.threatEngine.ThreatUtility._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Try
import scala.util.control.Breaks.{break, breakable}
import scala.xml.{Elem, MetaData, XML}

object BackgroundScreenshot {

  private val SET_FLAG_METHOD_PATTERN = ".*(add|set)Flags.*"
  private val SAFE_FLAG               = "WindowManager.LayoutParams.FLAG_SECURE"
  private val logger                  = LoggerFactory.getLogger(getClass)

  /** Check for violation for auto-generated background screenshot threat
    * @param cpg
    *   source filepath of manifest file
    * @return
    */
  def getViolations(cpg: Cpg): Try[(Boolean, List[Json])] = Try {
    // implicit for callIn
    implicit val resolver: ICallResolver = NoResolve
    if (hasDataElements(cpg)) {
      val safeFlagCalls = cpg.method
        .fullName(SET_FLAG_METHOD_PATTERN)
        .callIn
        .where(_.argument.code(s".*${SAFE_FLAG}.*"))

      // violation if empty
      (safeFlagCalls.isEmpty, List())
    } else (false, List())
  }
}
