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

import java.io.File

import scala.io.Source
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

abstract class ExternalProcessing {
  def process(a: Int, b: Int): Int
}

case class Input(a: Int, b: Int)

case class LoadExternalScalaFile(filePath: String) {
  val toolbox      = currentMirror.mkToolBox()
  val fileContents = Source.fromFile(filePath).getLines().mkString("\n")
  val tree         = toolbox.parse(s"import ai.privado.script._;$fileContents")
  val compiledCode = toolbox.compile(tree)

  def getFileReference: ExternalProcessing = compiledCode().asInstanceOf[ExternalProcessing]
}

class ExternalScalaFileHelper {
  def processExternalFile(filePath: String, data: Input): Either[String, Int] = {
    try {
      if (new File(filePath).exists()) {
        val externalFile       = LoadExternalScalaFile(filePath)
        val externalProcessing = externalFile.getFileReference
        Right(externalProcessing.process(data.a, data.b))
      } else Left("File doesn't exit")
    } catch {
      case ex: Exception => Left(ex.getMessage)
    }
  }
}
