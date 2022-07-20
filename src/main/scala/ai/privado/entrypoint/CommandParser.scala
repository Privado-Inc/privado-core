package ai.privado.entrypoint

import better.files.File
import scopt.OParser

import scala.sys.exit

case class Config(
  cmd: Set[String] = Set.empty,
  sourceFile: Set[String] = Set.empty,
  internalRulesPath: Set[String] = Set.empty,
  externalRulePath: Set[String] = Set.empty,
  ignoreInternalRules: Boolean = false,
  downladDependencies: Boolean = true
)

object CommandParser {
  def parse(args: Array[String]): Option[Config] = {
    val builder = OParser.builder[Config]

    val parser = {
      import builder._
      OParser.sequence(
        programName("privado-core"),
        head("privado-core", "*** TODO: Add version details***"),
        help("help").text("prints this usage text"),
        cmd("scan")
          .required()
          .action((_, c) => c.copy(cmd = c.cmd + "scan"))
          .text(
            "Scans the given source directory, identifies the privacy data elements, dataflows, collection points and generates compliance report."
          )
          .children(
            opt[String]("internal-rules")
              .abbr("ir")
              .required()
              .action((x, c) => c.copy(internalRulesPath = c.internalRulesPath + x))
              .text("Internal rule files location"),
            opt[String]("external-rules")
              .abbr("er")
              .optional()
              .action((x, c) => c.copy(externalRulePath = c.externalRulePath + x))
              .text("External rule files location"),
            opt[Boolean]("ignore-default-rules")
              .abbr("i")
              .optional()
              .action((_, c) => c.copy(ignoreInternalRules = true))
              .text("Ignore internal rules "),
            opt[Boolean]("skip-download-dependencies")
              .abbr("sdd")
              .optional()
              .action((_, c) => c.copy(downladDependencies = false))
              .text("this option is hidden in the usage text"),
            arg[String]("<Source directory>")
              .required()
              .action((x, c) => c.copy(sourceFile = c.sourceFile + x))
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
    val conf = OParser.parse(parser, args, Config())
    conf match {
      case Some(config) => Some(config)
      case _ =>
        println(OParser.usage(parser))
        exit(1)
    }
  }

}
