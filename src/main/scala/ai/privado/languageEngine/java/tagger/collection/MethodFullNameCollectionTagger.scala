package ai.privado.languageEngine.java.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, InternalTag, RuleInfo}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, Call, Method, MethodRef, Unknown}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import ai.privado.utility.Utilities.*

import scala.util.control.Breaks.{break, breakable}
import scala.util.{Failure, Success, Try}

class MethodFullNameCollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends CollectionTagger(cpg, ruleCache) {
  private val logger = LoggerFactory.getLogger(this.getClass)
  override def generateParts(): Array[RuleInfo] =
    // we want to look at methods, not annotations
    ruleCache.getRule.collections.filter(_.catLevelTwo == Constants.default).toArray

  override def runOnPart(builder: DiffGraphBuilder, ruleInfo: RuleInfo): Unit = {
    val methodsCache = scala.collection.mutable.HashMap.empty[Long, Method]
    methodsCache.addAll(collectUrlsFromHandlerEndpoints(ruleInfo.combinedRulePattern))
    tagCollectionEndpoints(builder, methodsCache.values.toList, ruleInfo)
  }

  private def tagCollectionEndpoints(
    builder: DiffGraphBuilder,
    collectionPoints: List[AstNode],
    collectionRuleInfo: RuleInfo,
    returnByName: Boolean = false
  ): Unit = {
    collectionPoints.foreach(collectionPoint => {
      // tag method
      addRuleTags(builder, collectionPoint, collectionRuleInfo, ruleCache)
      storeForTag(builder, collectionPoint, ruleCache)(
        InternalTag.COLLECTION_METHOD_ENDPOINT.toString,
        getFinalEndPoint(collectionPoint, returnByName)
      )
      // tag method's ast nodes
      collectionPoint.ast
        .where(_.tag.nameExact(Constants.id))
        .foreach(node =>
          storeForTag(builder, node, ruleCache)(
            InternalTag.COLLECTION_METHOD_ENDPOINT.toString,
            getFinalEndPoint(collectionPoint, false)
          )
        )
    })
  }

  private def getFinalEndPoint(collectionPoint: AstNode, returnByName: Boolean): String = {
    if (returnByName && collectionPoint.isMethod) {
      collectionPoint.asInstanceOf[Method].name
    } else {
      val methodUrl = methodUrlMap.getOrElse(collectionPoint.id(), "")
      Try(classUrlMap.getOrElse(collectionPoint.asInstanceOf[Method].typeDecl.head.id(), "")) match {
        case Success(classUrl) => classUrl + methodUrl
        case Failure(e) =>
          methodUrl
      }
    }
  }

  private def collectUrlsFromHandlerEndpoints(combinedRulePatterns: String): Map[Long, Method] = {
    val TYPE_FULL_NAME_CONST = "TYPE_FULL_NAME"
    val methodCalls          = cpg.call.methodFullName(combinedRulePatterns).l
    val methods              = scala.collection.mutable.HashMap.empty[Long, Method]
    val localMethodUrlMap    = scala.collection.mutable.HashMap.empty[Long, String]
    for (methodCall <- methodCalls) {
      val url                           = methodCall.argument.isLiteral.code.head
      var handlerMethod: Option[Method] = Option.empty[Method]

      breakable {
        if (methodCall.argument.length < 2) { // we do not have enough arguments to get the handler method
          break                               // empty handler, continue loop
        }
        // match on second argument, that's the handler
        methodCall.argument(2) match {
          case c: Call =>
            if (c.methodFullName == Operators.fieldAccess) { // E.g. AnotherHandlerClass.someHandler
              val children = c.astChildren.l
              // Anything but a field identifier is the method for us, Call, TypeRef, TypeDecl.
              val fieldMap = children.filter(!_.isFieldIdentifier).head.toMap
              if (fieldMap.isEmpty) {
                break // continue loop
              }

              val classNameOption: Option[Any] = fieldMap.get(TYPE_FULL_NAME_CONST)
              if (classNameOption.isEmpty) {
                break // continue loop
              }
              // Kotlin has a '$' in this typename, so getting everything before that
              val className  = classNameOption.get.toString.split('$').headOption.getOrElse("")
              val methodName = children.isFieldIdentifier.canonicalName.headOption
              if (className != "" && methodName.isDefined) {
                // get the assignment and the handler from there
                /** this.endpointHandler = { req: Request, res: Response -> "hello from handler" }
                  */
                handlerMethod = cpg.file
                  .where(_.typeDecl.fullName(s".*${className}.*"))
                  .ast
                  .isCall
                  .name(Operators.assignment)
                  .where(_.argument(1).code(s".*${methodName.get}.*"))
                  .argument(2)
                  .isMethodRef
                  .referencedMethod
                  .headOption
              }
            } else { // E.g. AnotherHandlerClass.anotherHandler()
              handlerMethod = c.callee.headOption
            }
          case m: MethodRef => // E.g. a code block like { req, res -> ... }
            handlerMethod = Some(m.referencedMethod)
          case u: Unknown => // E.g. this::someHandler or SomeHandlerClass::someOtherHandler
            handlerMethod = handleUnknownArgType(u)
          case err =>
            logger.error(s"Unexpected 2nd argument type while tagging collection: ${err.code}")
        }
      }
      if (handlerMethod.isDefined) {
        localMethodUrlMap += (handlerMethod.get.id() -> url)
        methods += (handlerMethod.get.id()           -> handlerMethod.get)
      }

    }
    methodUrlMap.addAll(localMethodUrlMap)
    methods.toMap
  }

  private def handleUnknownArgType(u: Unknown): Option[Method] = {
    var handlerMethod: Option[Method] = Option.empty[Method]
    val classAccessor                 = "::"
    // For kotlin or java - different parser type names
    val isKotlinMethodHandler = u.parserTypeName == "KtCallableReferenceExpression" // Kotlin
    val isJavaMethodHandler   = u.parserTypeName == "MethodReferenceExpr"           // Java
    if (isKotlinMethodHandler || isJavaMethodHandler) {
      // get the code part - full handler name
      val handlerName = u.code
      val thisPrefix  = "this" + classAccessor
      if (handlerName.contains(classAccessor)) {
        // method name is after the "::" part
        val methodName =
          handlerName.substring(handlerName.indexOf(classAccessor) + classAccessor.length, handlerName.length)
        if (handlerName.startsWith(thisPrefix)) { // this::someHandler - in the same class
          // Look in the same file
          handlerMethod = u.file.method.nameExact(methodName).dedup.headOption
        } else { // SomeClass::someMethod style handler - companion or static method
          // class name is before the "::" part
          val className = handlerName.substring(0, handlerName.indexOf(classAccessor))
          handlerMethod = cpg.method.fullName(s".*$className\\.$methodName.*").headOption
          if (isKotlinMethodHandler && handlerMethod.isEmpty) {
            handlerMethod = cpg.method.fullName(s".*$className\\$$Companion\\.$methodName.*").headOption
          }
        }
      }
    }
    handlerMethod
  }
}
