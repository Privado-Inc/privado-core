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
 *
 */

package ai.privado.script

import ai.privado.cache.RuleCache
import ai.privado.model.Constants
import dotty.tools.repl.ScriptEngine
import io.circe.Json
import io.shiftleft.codepropertygraph.Cpg

import java.io.File
import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.reflect.runtime.{currentMirror, universe}
import scala.tools.reflect.{FastTrack, ToolBox}
import scala.util.Using

abstract class ExternalScript {
  def process(cpg: Cpg, output: mutable.LinkedHashMap[String, Json]): Unit
}

case class LoadExternalScript(filePath: String) {
  private val sourceCode = Using(Source.fromFile(filePath)) { source => source.mkString }.getOrElse("")
  def getFileReference: ExternalScript =
    ScriptEngine().eval(s"import ai.privado.script.*\n$sourceCode").asInstanceOf[ExternalScript]
}

object ExternalScalaScriptRunner {
  def postExportTrigger(cpg: Cpg, ruleCache: RuleCache, output: mutable.LinkedHashMap[String, Json]): Unit = {
    try {
      val filePath = ruleCache.getSystemConfigByKey(Constants.postExportTrigger, raw = true)
      if (new File(filePath).exists()) {
        println(s"Executing post export script")
        val externalFile       = LoadExternalScript(filePath)
        val externalProcessing = externalFile.getFileReference
        externalProcessing.process(cpg, output)
        println("Post export script execution completed")
      } else if (filePath.nonEmpty)
        println(s"External script file: $filePath doesn't exist")
    } catch {
      case ex: Exception => println(ex.getMessage)
    }
  }
}
