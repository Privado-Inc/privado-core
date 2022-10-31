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

import ai.privado.metric.MetricHandler
import io.circe.syntax.EncoderOps
import scopt.OParser

import scala.sys.exit

case class PrivadoInput(
  cmd: Set[String] = Set.empty,
  sourceLocation: Set[String] = Set.empty,
  internalConfigPath: Set[String] = Set.empty,
  externalConfigPath: Set[String] = Set.empty,
  ignoreInternalRules: Boolean = false,
  skipDownloadDependencies: Boolean = false,
  disableDeDuplication: Boolean = false,
  ignoreExcludeRules: Boolean = false,
  skipUpload: Boolean = false,
  upload: Boolean = false
)

object CommandConstants {
  val SCAN                       = "scan"
  val INTERNAL_CONFIG            = "internal-config"
  val INTERNAL_CONFIG_ABBR       = "ic"
  val EXTERNAL_CONFIG            = "external-config"
  val EXTERNAL_CONFIG_ABBR       = "ec"
  val IGNORE_DEFAULT_RULES       = "ignore-default-rules"
  val IGNORE_DEFAULT_RULES_ABBR  = "i"
  val SKIP_DOWNLOAD_DEP          = "skip-download-dependencies"
  val SKIP_DOWNLOAD_DEP_ABBR     = "sdd"
  val DISABLE_DEDUPLICATION      = "disable-deduplication"
  val DISABLE_DEDUPLICATION_ABBR = "dd"
  val IGNORE_EXCLUDE_RULES       = "ignore-exclude-rules"
  val IGNORE_EXCLUDE_RULES_ABBR  = "ier"
  val UPLOAD                     = "upload"
  val UPLOAD_ABBR                = "u"
  val SKIP_UPLOAD                = "skip-upload"
  val SKIP_UPLOAD_ABBR           = "su"
  val VALIDATE                   = "validate"
}

object CommandParser {
  val commandMapper: Map[String, CommandProcessor] =
    Map(
      CommandConstants.SCAN     -> ScanProcessor,
      CommandConstants.UPLOAD   -> UploadProcessor,
      CommandConstants.VALIDATE -> RuleValidator
    )
  def parse(args: Array[String]): Option[CommandProcessor] = {
    val builder = OParser.builder[PrivadoInput]

    val parser = {
      import builder._
      OParser.sequence(
        programName("privado-core"),
        head("privado-core", "*** TODO: Add version details***"),
        help("help").text("prints this usage text"),
        cmd(CommandConstants.SCAN)
          .required()
          .action((_, c) => c.copy(cmd = c.cmd + CommandConstants.SCAN))
          .text(
            "Scans the given source directory, identifies the privacy data elements, dataflows, collection points and generates compliance report."
          )
          .children(
            opt[String](CommandConstants.INTERNAL_CONFIG)
              .abbr(CommandConstants.INTERNAL_CONFIG_ABBR)
              .required()
              .action((x, c) => c.copy(internalConfigPath = c.internalConfigPath + x))
              .text("Internal config and rule files location"),
            opt[String](CommandConstants.EXTERNAL_CONFIG)
              .abbr(CommandConstants.EXTERNAL_CONFIG_ABBR)
              .optional()
              .action((x, c) => c.copy(externalConfigPath = c.externalConfigPath + x))
              .text("External config and rule files location"),
            opt[Unit](CommandConstants.IGNORE_DEFAULT_RULES)
              .abbr(CommandConstants.IGNORE_DEFAULT_RULES_ABBR)
              .optional()
              .action((_, c) => c.copy(ignoreInternalRules = true))
              .text("Ignore internal rules "),
            opt[Unit](CommandConstants.SKIP_DOWNLOAD_DEP)
              .abbr(CommandConstants.SKIP_DOWNLOAD_DEP_ABBR)
              .optional()
              .action((_, c) => c.copy(skipDownloadDependencies = true))
              .text("this option is hidden in the usage text"),
            opt[Unit](CommandConstants.DISABLE_DEDUPLICATION)
              .abbr(CommandConstants.DISABLE_DEDUPLICATION_ABBR)
              .optional()
              .action((_, c) => c.copy(disableDeDuplication = true))
              .text("Disable De-Duplication of dataflow"),
            opt[Unit](CommandConstants.IGNORE_EXCLUDE_RULES)
              .abbr(CommandConstants.IGNORE_EXCLUDE_RULES_ABBR)
              .optional()
              .action((_, c) => c.copy(ignoreExcludeRules = true))
              .text("Ignore source exclude rules"),
            opt[Unit](CommandConstants.SKIP_UPLOAD)
              .abbr(CommandConstants.SKIP_UPLOAD_ABBR)
              .optional()
              .action((_, c) => c.copy(skipUpload = true))
              .text(
                "Skip the output result getting uploaded to cloud without being prompted (if you have not given the consent for the same). You can skip the upload if you have already given such consent"
              ),
            opt[Unit](CommandConstants.UPLOAD)
              .abbr(CommandConstants.UPLOAD_ABBR)
              .optional()
              .action((_, c) => c.copy(upload = true))
              .text(
                "Upload the output results to cloud without being prompted for the consent. If you have already given the consent this flag has no effect"
              ),
            arg[String]("<Source directory>")
              .required()
              .action((x, c) => c.copy(sourceLocation = c.sourceLocation + x))
              .text("Source code location"),
            checkConfig(c =>
              if (c.cmd.isEmpty) failure("")
              else if (c.ignoreInternalRules && c.externalConfigPath.isEmpty)
                failure("external rule files location is required if you ignore the internal rules")
              else success
            )
          ),
        cmd(CommandConstants.VALIDATE)
          .required()
          .action((_, c) => c.copy(cmd = c.cmd + CommandConstants.VALIDATE))
          .text("Validates all rules included inside the given rules directory. Informs in case of invalid rules")
          .children(
            arg[String]("<config directory>")
              .required()
              .action((x, c) => c.copy(externalConfigPath = c.externalConfigPath + x))
              .text("Config directory location"),
            checkConfig(c =>
              if (c.cmd.isEmpty) failure("")
              else success
            )
          ),
        cmd(CommandConstants.UPLOAD)
          .required()
          .action((_, c) => c.copy(cmd = c.cmd + CommandConstants.UPLOAD))
          .text("Uploads the result file to Privado.ai UI dashboard.")
          .children(
            arg[String]("<Source directory>")
              .required()
              .action((x, c) => c.copy(sourceLocation = c.sourceLocation + x))
              .text("Source code location"),
            checkConfig(c =>
              if (c.cmd.isEmpty) failure("")
              else success
            )
          )
      )
    }
    val conf = OParser.parse(parser, args, PrivadoInput())
    conf match {
      case Some(config) =>
        val commandProcessor: CommandProcessor = commandMapper.get(config.cmd.head) match {
          case Some(commandProcessor) => {
            MetricHandler.metricsData("privadoCoreCommand") = config.cmd.head.asJson
            commandProcessor
          }
          case _ =>
            println(OParser.usage(parser))
            exit(1)
        }
        commandProcessor.config = config
        Some(commandProcessor)
      case _ =>
        println(OParser.usage(parser))
        exit(1)
    }
  }

}
