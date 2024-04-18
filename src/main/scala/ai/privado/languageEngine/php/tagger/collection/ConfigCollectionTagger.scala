package ai.privado.languageEngine.php.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.languageEngine.java.tagger.collection.CollectionUtility
import ai.privado.model.{CatLevelOne, Constants, FilterProperty, InternalTag, Language, NodeType, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.storeForTag
import better.files.File.VisitOptions
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language.*

import ai.privado.utility.Utilities.resolver

import scala.collection.mutable
import io.circe.*
import io.circe.generic.auto.*
import io.circe.yaml
import io.shiftleft.codepropertygraph.generated.nodes.Method

case class Route(path: String, defaults: Option[Defaults], controller: Option[String])
case class Defaults(_controller: String)

case class ControllerAlias(clazz: String)

class ConfigCollectionTagger(cpg: Cpg, ruleCache: RuleCache, projectRoot: String)
    extends PrivadoParallelCpgPass[Route](cpg) {

  private val configExtensions: Set[String]                 = Set(".yaml", ".yml")
  protected val methodUrlMap: mutable.HashMap[Long, String] = mutable.HashMap[Long, String]()
  protected val classUrlMap: mutable.HashMap[Long, String]  = mutable.HashMap[Long, String]()
  private val logger                                        = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[Route] = {
    SourceFiles
      .determine(projectRoot, configExtensions, ignoredFilesRegex = Some(".*[.]privado.*".r))(VisitOptions.default)
      .filter(_.contains(Constants.routes))
      .flatMap(getRoutesFromConfig)
      .toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, route: Route): Unit = {
    val (controllerClassName, controllerMethodName) = getClassAndMethodNameFromControllerFullName(
      route.controller
        .getOrElse(
          findControllerFullNameFromAlias(route.defaults.getOrElse(Defaults(""))._controller.replaceFirst(":", ""))
        )
    )

    methodUrlMap.addOne(
      (
        cpg.method.nameExact(controllerMethodName).headOption match {
          case Some(method) => method.id()
          case None =>
            cpg.typeDecl.nameExact(controllerClassName).astChildren.isMethod.nameExact("__invoke").headOption match {
              case Some(invokeMethod) => invokeMethod.id()
              case None               => -1
            }
        },
        route.path
      )
    )

    // The RuleInfo is intended to be empty
    tagSources(
      builder,
      RuleInfo(
        "",
        "",
        "",
        FilterProperty.CODE,
        Array.empty[String],
        List.empty[String],
        false,
        "",
        Map.empty[String, String],
        NodeType.UNKNOWN,
        "",
        CatLevelOne.COLLECTIONS,
        catLevelTwo = Constants.annotations,
        Language.PHP,
        Array()
      ),
      methodUrlMap.flatMap((id, _) => cpg.method.id(id)).toList
    )
  }

  /** Parses a YAML configuration file to extract defined api routes, along with their corresponding controllers.
    *
    * @param fileName
    *   The path to the YAML configuration file.
    * @return
    *   An iterable collection of Route objects extracted from the configuration file.
    */
  private def getRoutesFromConfig(fileName: String): Iterable[Route] = {
    yaml.parser
      .parse(better.files.File(fileName).contentAsString)
      .map { parserOp =>
        parserOp.asObject
          .getOrElse(JsonObject.empty)
          .values
          .map(value => {
            val emptyRoute = Route("", Option(Defaults("")), Option(""))
            value.as[Route].getOrElse(emptyRoute)
          })
      }
      .getOrElse(Seq.empty[Route])
  }

  protected def tagSources(
    builder: DiffGraphBuilder,
    ruleInfo: RuleInfo,
    collectionMethodsCache: List[Method]
  ): Unit = {
    tagDirectSources(cpg, builder, collectionMethodsCache, ruleInfo)

    CollectionUtility.tagDerivedSources(
      cpg,
      builder,
      collectionMethodsCache,
      ruleInfo,
      ruleCache,
      methodUrlMap = methodUrlMap,
      classUrlMap = classUrlMap
    )
  }

  /** Extracts class name and method name from a fully qualified controller class name. If no method name is explicitly
    * specified, fallback would be the `__invoke` method.
    *
    * @param controllerClass
    *   The fully qualified controller class name.
    * @return
    *   A tuple containing the class name and method name.
    */
  private def getClassAndMethodNameFromControllerFullName(controllerClass: String): (String, String) = {
    val classMethodSplit = controllerClass.split("::")

    val controllerClassName =
      classMethodSplit.headOption.getOrElse("").split("\\\\").lastOption.getOrElse("")

    val controllerMethodName = if (classMethodSplit.length > 1) classMethodSplit.last else "__invoke"

    (controllerClassName, controllerMethodName)
  }

  private def findControllerFullNameFromAlias(alias: String): String = {
    SourceFiles
      .determine(projectRoot, configExtensions, ignoredFilesRegex = Some(".*[.]privado.*".r))(VisitOptions.default)
      .filter(_.contains("controller"))
      .map(fileName => parseConfigToGetControllerFullName(fileName, alias))
      .headOption
      .getOrElse("")
  }

  /** Parses `yaml` files which contains controller in their name and finds the fully qualified controller class name
    * from the alias found in the route file.
    *
    * @param fileName
    *   The path to the configuration file.
    * @param alias
    *   The alias of the controller.
    * @return
    *   The fully qualified controller class name if found, otherwise an empty string.
    */
  private def parseConfigToGetControllerFullName(fileName: String, alias: String): String = {
    yaml.parser
      .parse(better.files.File(fileName).contentAsString)
      .map(parsedOp => {
        parsedOp.findAllByKey(alias) match {
          case List(someAlias) => someAlias.findAllByKey("class").map(_.asString.headOption.getOrElse(""))
          case _               => List.empty[String]
        }
      }) match {
      case Left(failure) => {
        logger.debug("Failed to parse yaml file containing controller aliases.")
        ""
      }
      case Right(someList) => {
        someList.headOption.getOrElse("")
      }
    }
  }

  private def tagDirectSources(
    cpg: Cpg,
    builder: DiffGraphBuilder,
    collectionMethods: List[Method],
    collectionRuleInfo: RuleInfo
  ): Unit = {
    val collectionPoints = collectionMethods.flatMap(collectionMethod => {
      ruleCache.getRule.sources.flatMap(sourceRule => {
        val parameters = collectionMethod.parameter
        val locals     = collectionMethod.local
        val literals   = collectionMethod.call("(?:get).*").argument.isLiteral

        // TODO: handle cases where `request.args.get('id', None)` used directly in handler block without method param
        val matchingParameters = parameters.where(_.name(sourceRule.combinedRulePattern)).whereNot(_.code("self")).l
        val matchingLocals     = locals.code(sourceRule.combinedRulePattern).l
        val matchingLiterals = literals
          .code("(\"|'|`)(" + sourceRule.combinedRulePattern + ")(\"|'|`)")
          .whereNot(_.code(".*\\s.*"))
          .l

        if (!(matchingParameters.isEmpty && matchingLocals.isEmpty && matchingLiterals.isEmpty)) {
          matchingParameters.foreach(parameter =>
            storeForTag(builder, parameter, ruleCache)(Constants.id, sourceRule.id)
          )
          matchingLocals.foreach(local => storeForTag(builder, local, ruleCache)(Constants.id, sourceRule.id))
          matchingLiterals.foreach(literal => storeForTag(builder, literal, ruleCache)(Constants.id, sourceRule.id))
          Some(collectionMethod)
        } else {
          None
        }
      })
    })

    CollectionUtility.tagMethodEndpoints(
      builder,
      collectionPoints.l,
      collectionRuleInfo,
      ruleCache,
      false,
      methodUrlMap,
      classUrlMap
    )
  }
}
