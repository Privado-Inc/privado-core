package ai.privado.languageEngine.ruby.passes

import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.astcreation.{AstCreator, ResourceManagedParser}
import io.joern.rubysrc2cpg.utils.{PackageContext, PackageTable}
import io.joern.x2cpg.SourceFiles
import io.joern.x2cpg.datastructures.Global
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.{ConcurrentWriterCpgPass, SimpleCpgPass}
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters.EnumerationHasAsScala

class AstCreationPassPrivado(
  cpg: Cpg,
  global: Global,
  parser: ResourceManagedParser,
  packageTable: PackageTable,
  config: Config
) extends SimpleCpgPass(cpg) {

  private val logger                        = LoggerFactory.getLogger(this.getClass)
  val RubySourceFileExtensions: Set[String] = Set(".rb")

  def allUsedTypes(): List[String] =
    global.usedTypes.keys().asScala.toList

  def run(diffGraph: DiffGraphBuilder): Unit = {
    val files = SourceFiles
      .determine(config.inputPath, RubySourceFileExtensions, config)
    println(s"Total no of files getting scanned ------ > ${files.size}")
    files
      .foreach(fileName => {
        try {
          diffGraph.absorb(
            new AstCreator(
              fileName,
              global,
              parser,
              PackageContext(fileName, packageTable),
              cpg.metaData.root.headOption
            )(config.schemaValidation)
              .createAst()
          )
        } catch {
          case ex: Exception =>
            logger.error(s"Error while processing AST for file - $fileName - ", ex)
        }
      })
  }
}
