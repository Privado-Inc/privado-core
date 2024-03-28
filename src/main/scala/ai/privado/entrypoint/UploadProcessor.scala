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

package ai.privado.entrypoint

import ai.privado.auth.AuthenticationHandler
import ai.privado.cache.{AppCache, Environment}
import ai.privado.model.Constants
import ai.privado.model.Constants.{outputDirectoryName, outputFileName}
import better.files.File
import org.slf4j.LoggerFactory

object UploadProcessor extends CommandProcessor {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def checkOutputFileExists(): Boolean = {
    val repoPath = config.sourceLocation.head
    File(s"$repoPath/$outputDirectoryName/$outputFileName").exists
  }

  override def process(appCache: AppCache): Either[String, Unit] = {
    println(s"Privado CLI Version: ${Environment.privadoVersionCli.getOrElse(Constants.notDetected)}")
    println(s"Privado Core Version: ${Environment.privadoVersionCore}")
    println(s"Synchronizing results with Privado Cloud...")
    checkOutputFileExists() match {
      case true =>
        Right(println(AuthenticationHandler.pushDataToCloud(config.sourceLocation.head)))
      case false =>
        logger.debug("Output file not found")
        println("Output file does not exist. Please Scan the repository to upload the file!")
        Left("Output file does not exist.")
    }
  }
}
