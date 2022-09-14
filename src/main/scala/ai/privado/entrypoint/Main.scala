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
import ai.privado.metric.MetricHandler
import org.slf4j.LoggerFactory

/** Privado Core main entry point
  */
object Main {
  private val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {

    CommandParser.parse(args) match {
      case Some(processor) =>
        try {
          MetricHandler.timeMetric(processor.process(), "Complete") match {
            case Right(_) =>
              processor match {
                case ScanProcessor =>
                  logger.debug("Success from scan process! Proceeding to initiate auth flow")
                  val sourceRepoLocation = ScanProcessor.config.sourceLocation.head
                  if (!ScanProcessor.config.skipUpload)
                    AuthenticationHandler.authenticate(sourceRepoLocation)
                case _ => ()
              }

            // raise error in case of failure, and collect
            // all handled & unhandled exceptions in catch
            case Left(err) =>
              MetricHandler.scanProcessErrors.addOne(err)
              throw new Exception(err)
          }
        } catch {
          case e: Exception =>
            // any user-facing non-debug logging to be done internally
            logger.debug("Failure from scan process:", e)
            logger.debug("Skipping auth flow due to scan failure")
        } finally {
          MetricHandler.compileAndSend()
        }
      case _ =>
      // arguments are bad, error message should get displayed from inside CommandParser.parse
    }

  }
}
