package ai.privado.entrypoint

import scopt.OParser

import scala.sys.exit

case class PrivadoInput(
  cmd: Set[String] = Set.empty,
  sourceLocation: Set[String] = Set.empty,
  internalRulesPath: Set[String] = Set.empty,
  externalRulePath: Set[String] = Set.empty,
  ignoreInternalRules: Boolean = false,
  skipDownladDependencies: Boolean = false,
  disableDeDuplication: Boolean = false
)

object CommandConstants {
  val SCAN                       = "scan"
  val INTERNAL_RULES             = "internal-rules"
  val INTERNAL_RULES_ABBR        = "ir"
  val EXTERNAL_RULES             = "external-rules"
  val EXTERNAL_RULES_ABBR        = "er"
  val IGNORE_DEFAULT_RULES       = "ignore-default-rules"
  val IGNORE_DEFAULT_RULES_ABBR  = "i"
  val SKIP_DOWNLOAD_DEP          = "skip-download-dependencies"
  val SKIP_DOWNLOAD_DEP_ABBR     = "sdd"
  val DISABLE_DEDUPLICATION      = "disable-deduplication"
  val DISABLE_DEDUPLICATION_ABBR = "dd"
}

object CommandParser {
  val commandMapper: Map[String, CommandProcessor] = Map(CommandConstants.SCAN -> ScanProcessor)
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
            opt[String](CommandConstants.INTERNAL_RULES)
              .abbr(CommandConstants.INTERNAL_RULES_ABBR)
              .required()
              .action((x, c) => c.copy(internalRulesPath = c.internalRulesPath + x))
              .text("Internal rule files location"),
            opt[String](CommandConstants.EXTERNAL_RULES)
              .abbr(CommandConstants.EXTERNAL_RULES_ABBR)
              .optional()
              .action((x, c) => c.copy(externalRulePath = c.externalRulePath + x))
              .text("External rule files location"),
            opt[Unit](CommandConstants.IGNORE_DEFAULT_RULES)
              .abbr(CommandConstants.IGNORE_DEFAULT_RULES_ABBR)
              .optional()
              .action((_, c) => c.copy(ignoreInternalRules = true))
              .text("Ignore internal rules "),
            opt[Unit](CommandConstants.SKIP_DOWNLOAD_DEP)
              .abbr(CommandConstants.SKIP_DOWNLOAD_DEP_ABBR)
              .optional()
              .action((_, c) => c.copy(skipDownladDependencies = true))
              .text("this option is hidden in the usage text"),
            opt[Unit](CommandConstants.DISABLE_DEDUPLICATION)
              .abbr(CommandConstants.DISABLE_DEDUPLICATION_ABBR)
              .optional()
              .action((_, c) => c.copy(disableDeDuplication = true))
              .text("Disable De-Duplication of dataflow"),
            arg[String]("<Source directory>")
              .required()
              .action((x, c) => c.copy(sourceLocation = c.sourceLocation + x))
              .text("Source code location"),
            checkConfig(c =>
              if (c.cmd.isEmpty) failure("")
              else if (c.ignoreInternalRules && c.externalRulePath.isEmpty)
                failure("external rule files location is required if you ignore the internal rules")
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
            print(commandProcessor)
            print(config.cmd.head)
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
