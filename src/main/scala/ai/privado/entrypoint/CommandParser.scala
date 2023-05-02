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
  disableRunTimeSemantics: Boolean = false,
  disableThisFiltering: Boolean = false,
  disableFlowSeparationByDataElement: Boolean = false,
  disable2ndLevelClosure: Boolean = false,
  enableAPIDisplay: Boolean = false,
  ignoreExcludeRules: Boolean = false,
  ignoreSinkSkipRules: Boolean = false,
  skipUpload: Boolean = false,
  upload: Boolean = false,
  testOutput: Boolean = false,
  showUnresolvedFunctionsReport: Boolean = false,
  generateAuditReport: Boolean = false,
  enableAuditSemanticsFilter: Boolean = false
)

object CommandConstants {
  val SCAN                                         = "scan"
  val INTERNAL_CONFIG                              = "internal-config"
  val INTERNAL_CONFIG_ABBR                         = "ic"
  val EXTERNAL_CONFIG                              = "external-config"
  val EXTERNAL_CONFIG_ABBR                         = "ec"
  val IGNORE_DEFAULT_RULES                         = "ignore-default-rules"
  val IGNORE_DEFAULT_RULES_ABBR                    = "i"
  val SKIP_DOWNLOAD_DEP                            = "skip-download-dependencies"
  val SKIP_DOWNLOAD_DEP_ABBR                       = "sdd"
  val DISABLE_DEDUPLICATION                        = "disable-deduplication"
  val DISABLE_DEDUPLICATION_ABBR                   = "dd"
  val DISABLE_RUNTIME_SEMANTICS                    = "disable-runtime-semantics"
  val DISABLE_RUNTIME_SEMANTICS_ABBR               = "drs"
  val DISABLE_THIS_FILTERING                       = "disable-this-filtering"
  val DISABLE_THIS_FILTERING_ABBR                  = "dtf"
  val DISABLE_FLOW_SEPERATION_BY_DATA_ELEMENT      = "disable-flow-separation-by-data-element"
  val DISABLE_FLOW_SEPERATION_BY_DATA_ELEMENT_ABBR = "dfsde"
  val DISABLE_2ND_LEVEL_CLOSURE                    = "disable-2nd-level-closure"
  val DISABLE_2ND_LEVEL_CLOSURE_ABBR               = "d2lc"
  val ENABLE_API_DISPLAY                           = "enable-api-display"
  val ENABLE_API_DISPLAY_ABBR                      = "ead"
  val IGNORE_EXCLUDE_RULES                         = "ignore-exclude-rules"
  val IGNORE_EXCLUDE_RULES_ABBR                    = "ier"
  val UPLOAD                                       = "upload"
  val UPLOAD_ABBR                                  = "u"
  val SKIP_UPLOAD                                  = "skip-upload"
  val SKIP_UPLOAD_ABBR                             = "su"
  val VALIDATE                                     = "validate"
  val UNRESOLVED_REPORT                            = "unresolved_report"
  val UNRESOLVED_REPORT_ABBR                       = "ur"
  val TEST_OUTPUT                                  = "test-output"
  val TEST_OUTPUT_ABBR                             = "tout"
  val GENERATE_AUDIT_REPORT                        = "generate-audit-report"
  val GENERATE_AUDIT_REPORT_ABBR                   = "gar"
  val ENABLE_AUDIT_SEMANTIC_FILTER                 = "enable-audit-semantic"
  val ENABLE_AUDIT_SEMANTIC_FILTER_ABBR            = "eas"
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
            opt[Unit](CommandConstants.DISABLE_RUNTIME_SEMANTICS)
              .abbr(CommandConstants.DISABLE_RUNTIME_SEMANTICS_ABBR)
              .optional()
              .action((_, c) => c.copy(disableRunTimeSemantics = true))
              .text("Disable runtime semantics"),
            opt[Unit](CommandConstants.DISABLE_THIS_FILTERING)
              .abbr(CommandConstants.DISABLE_THIS_FILTERING_ABBR)
              .optional()
              .action((_, c) => c.copy(disableThisFiltering = true))
              .text("Disable 'this' filtering"),
            opt[Unit](CommandConstants.DISABLE_FLOW_SEPERATION_BY_DATA_ELEMENT)
              .abbr(CommandConstants.DISABLE_FLOW_SEPERATION_BY_DATA_ELEMENT_ABBR)
              .optional()
              .action((_, c) => c.copy(disableFlowSeparationByDataElement = true))
              .text("Disable flow separation by data element"),
            opt[Unit](CommandConstants.DISABLE_2ND_LEVEL_CLOSURE)
              .abbr(CommandConstants.DISABLE_2ND_LEVEL_CLOSURE_ABBR)
              .optional()
              .action((_, c) => c.copy(disable2ndLevelClosure = true))
              .text("Disable 2nd level closure"),
            opt[Unit](CommandConstants.ENABLE_API_DISPLAY)
              .abbr(CommandConstants.ENABLE_API_DISPLAY_ABBR)
              .optional()
              .action((_, c) => c.copy(enableAPIDisplay = true))
              .text("Enable api display"),
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
            opt[Unit](CommandConstants.UNRESOLVED_REPORT)
              .abbr(CommandConstants.UNRESOLVED_REPORT_ABBR)
              .optional()
              .action((_, c) => c.copy(showUnresolvedFunctionsReport = true))
              .text("Calculate %age of unresolved function namespaces/signatures"),
            opt[Unit](CommandConstants.UPLOAD)
              .abbr(CommandConstants.UPLOAD_ABBR)
              .optional()
              .action((_, c) => c.copy(upload = true))
              .text(
                "Upload the output results to cloud without being prompted for the consent. If you have already given the consent this flag has no effect"
              ),
            opt[Unit](CommandConstants.TEST_OUTPUT)
              .abbr(CommandConstants.TEST_OUTPUT_ABBR)
              .optional()
              .action((_, c) => c.copy(testOutput = true))
              .text("Export the intermediate flow output"),
            opt[Unit](CommandConstants.GENERATE_AUDIT_REPORT)
              .abbr(CommandConstants.GENERATE_AUDIT_REPORT_ABBR)
              .optional()
              .action((_, c) => c.copy(generateAuditReport = true))
              .text("Export the audit report"),
            opt[Unit](CommandConstants.ENABLE_AUDIT_SEMANTIC_FILTER)
              .abbr(CommandConstants.ENABLE_AUDIT_SEMANTIC_FILTER_ABBR)
              .optional()
              .action((_, c) => c.copy(enableAuditSemanticsFilter = true))
              .text("Enable semantic filter in dataflow audit report"),
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
