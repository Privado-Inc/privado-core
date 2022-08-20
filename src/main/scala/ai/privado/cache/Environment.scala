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

package ai.privado.cache

import ai.privado.model.Constants
import org.slf4j.LoggerFactory

import scala.io.Source

object Environment {
  private val logger = LoggerFactory.getLogger(this.getClass)

  val userHash: Option[String]          = sys.env.get("PRIVADO_USER_HASH")
  val dockerAccessKey: Option[String]   = sys.env.get("PRIVADO_DOCKER_ACCESS_KEY")
  val syncToCloud: Option[String]       = sys.env.get("PRIVADO_SYNC_TO_CLOUD")
  val metricsEnabled: Option[String]    = sys.env.get("PRIVADO_METRICS_ENABLED")
  val isProduction: Option[String]      = sys.env.get("IS_PRODUCTION")
  val sessionId: Option[String]         = sys.env.get("PRIVADO_SESSION_ID")
  val hostScanDirectory: Option[String] = sys.env.get("PRIVADO_HOST_SCAN_DIR")
  val privadoVersionCli: Option[String] = sys.env.get("PRIVADO_VERSION_CLI")
  val privadoVersionCore = {
    var version = Constants.notDetected
    try {
      val lines = Source.fromResource("version.txt").getLines()
      for (line <- lines) {
        version = line
      }
    } catch {
      case e: Exception =>
        logger.error("Error fetching version for privado-core")
        logger.debug("Error occurred ", e)
    }
    version
  }
}
