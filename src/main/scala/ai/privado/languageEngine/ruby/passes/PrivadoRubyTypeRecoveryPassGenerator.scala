package ai.privado.languageEngine.ruby.passes

import ai.privado.model.Constants
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.joern.x2cpg.passes.frontend.*
import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromNodeExt
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.Defines.{ConstructorMethodName, DynamicCallUnknownFullName}
import io.joern.x2cpg.Defines as XDefines
import io.joern.x2cpg.passes.frontend.SBKey.getClass
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, Operators, PropertyNames}
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.{Assignment, FieldAccess}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromNodeExt
import org.slf4j.{Logger, LoggerFactory}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.Try
object SBKeyPrivado {
  protected val logger: Logger = LoggerFactory.getLogger(getClass)
  def fromNodeToLocalKey(node: AstNode): Option[LocalKey] = {
    Option(node match {
      case n: Identifier => LocalVar(n.name)
      case n: Local      => LocalVar(n.name)
      case n: Call =>
        CallAlias(
          n.name,
          n.argument.collectFirst {
            case x: Identifier if x.argumentIndex == 0 => x.name
            case c: Call if c.argumentIndex == 0 && c.name == Constants.scopeResolutionOperator =>
              c.code.stripPrefix("::").trim
          }
        )
      case n: Method            => CallAlias(n.name, Option("this"))
      case n: MethodRef         => CallAlias(n.code)
      case n: FieldIdentifier   => LocalVar(n.canonicalName)
      case n: MethodParameterIn => LocalVar(n.name)
      case _ => logger.debug(s"Local node of type ${node.label} is not supported in the type recovery pass."); null
    })
  }

}

class PrivadoRubyTypeRecoveryPassGenerator(
  cpg: Cpg,
  globalSymbolTable: SymbolTable[LocalKey],
  config: XTypeRecoveryConfig = XTypeRecoveryConfig()
) extends XTypeRecoveryPassGenerator[File](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState, iteration: Int): XTypeRecovery[File] =
    new RubyTypeRecovery(cpg, globalSymbolTable, state, iteration)
}

private class RubyTypeRecovery(
  cpg: Cpg,
  globalSymbolTable: SymbolTable[LocalKey],
  state: XTypeRecoveryState,
  iteration: Int
) extends XTypeRecovery[File](cpg, state, iteration) {

  override def compilationUnits: Traversal[File] = cpg.file.iterator

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] = {
    new RecoverForRubyFile(cpg, globalSymbolTable, unit, builder, state)
  }
}

private class RecoverForRubyFile(
  cpg: Cpg,
  globalSymbolTable: SymbolTable[LocalKey],
  cu: File,
  builder: DiffGraphBuilder,
  state: XTypeRecoveryState
) extends RecoverForXCompilationUnit[File](cpg, cu, builder, state) {

  import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromNodeExt

  override protected val symbolTable = new SymbolTable[LocalKey](SBKeyPrivado.fromNodeToLocalKey)

  /** A heuristic method to determine if a call is a constructor or not.
    */
  override protected def isConstructor(c: Call): Boolean = {
    isConstructor(c.name)
  }

  /** A heuristic method to determine if a call name is a constructor or not.
    */
  override protected def isConstructor(name: String): Boolean =
    !name.isBlank && (name.equals("new") || name.equals("<init>"))

  override def visitImport(i: Import): Unit = for {
    resolvedImport <- i.call.tag
    alias          <- i.importedAs
  } {
    import io.shiftleft.semanticcpg.language.importresolver._
    EvaluatedImport.tagToEvaluatedImport(resolvedImport).foreach {
      case ResolvedTypeDecl(fullName, _) =>
        symbolTable.append(LocalVar(fullName.split("\\.").lastOption.getOrElse(alias)), fullName)
      case _ => super.visitImport(i)
    }
  }

  override def methodReturnValues(methodFullNames: Seq[String]): Set[String] = {
    // Check if we have a corresponding member to resolve type
    val memberTypes = methodFullNames.flatMap { fullName =>
      val memberName = fullName.split("\\.").lastOption
      if (memberName.isDefined) {
        val typeDeclFullName = fullName.stripSuffix(s".${memberName.get}")
        cpg.typeDecl.fullName(typeDeclFullName).member.nameExact(memberName.get).typeFullName.l
      } else List.empty
    }.toSet
    if (memberTypes.nonEmpty) memberTypes
    else {
      val rs = cpg.method
        .fullNameExact(methodFullNames.toList: _*)
        .flatMap(m => Try(m.methodReturn).toOption)
        .flatMap(mr => mr.typeFullName +: mr.dynamicTypeHintFullName)
        .filterNot(_.equals("ANY"))
        .toSet
      if (rs.isEmpty) methodFullNames.map(_.concat(s"$pathSep${XTypeRecovery.DummyReturnType}")).toSet
      else rs
    }
  }

  override def visitIdentifierAssignedToCall(i: Identifier, c: Call): Set[String] = {
    if (c.name.startsWith("<operator>")) {
      visitIdentifierAssignedToOperator(i, c, c.name)
    } else if (symbolTable.contains(c) || globalSymbolTable.contains(c)) {
      visitIdentifierAssignedToCallRetVal(i, c)
    } else if (isCallHeadArgumentAScopeResolutionAndIsLastArgumentInTable(c)) {
      setCallMethodFullNameFromBaseScopeResolution(c)
      // Repeat this method now that the call has a type
      visitIdentifierAssignedToCall(i, c)
    } else if (c.argument.headOption.exists(arg => symbolTable.contains(arg) || globalSymbolTable.contains(arg))) {
      setCallMethodFullNameFromBase(c)
      // Repeat this method now that the call has a type
      visitIdentifierAssignedToCall(i, c)
    } else {
      // We can try obtain a return type for this call
      visitIdentifierAssignedToCallRetVal(i, c)
    }
  }

  /** Return true if `methodFullName` after the `:program` part matches the callCode excluding arguments
    * @param methodFullName
    * @param callCode
    * @return
    */
  def isCallParentScopeResolutionMatching(methodFullName: String, callCode: String) = {
    try {
      val cNameList = methodFullName.split(":program").last.split("\\.").filterNot(_.isEmpty)
      val codeList  = callCode.split("\\(").head.split("[:.]").filterNot(_.isEmpty).dropRight(1).toList
      cNameList sameElements codeList
    } catch {
      case e: Exception => false
    }

  }

  /** Return true if the passed node is `foo` and it is called as `Pay::Braintree.Billable.foo()`,
    *
    * Here we check whether the head argument is `ScopeResolution` (which is true above) and is `Billable` present in
    * symbol table which accessor as `Pay.Braintree`
    *
    * If the above conditions hold true, return true else false
    * @param c
    * @return
    */
  def isCallHeadArgumentAScopeResolutionAndIsLastArgumentInTable(c: Call): Boolean = c.argument.headOption
    .exists(_.isCall) && c.argument.head
    .asInstanceOf[Call]
    .name
    .equals(Constants.scopeResolutionOperator) && c.argument.head
    .asInstanceOf[Call]
    .argument
    .lastOption
    .exists(arg =>
      symbolTable.get(arg).union(globalSymbolTable.get(arg)).exists(isCallParentScopeResolutionMatching(_, c.code))
    )

  protected def setCallMethodFullNameFromBaseScopeResolution(c: Call): Set[String] = {
    val recTypes = c.argument.headOption
      .map {
        case x: Call if x.name.equals(Constants.scopeResolutionOperator) =>
          x.argument.lastOption
            .map(i =>
              symbolTable.get(i).union(globalSymbolTable.get(i)).filter(isCallParentScopeResolutionMatching(_, c.code))
            )
            .getOrElse(Set.empty[String])
            .map(_.concat(s"$pathSep${c.name}"))
      }
      .getOrElse(Set.empty[String])
    symbolTable.append(c, recTypes)
  }

  private def debugLocation(n: AstNode): String = {
    val fileName = n.file.name.headOption.getOrElse("<unknown>").stripPrefix(codeRoot)
    val lineNo   = n.lineNumber.getOrElse("<unknown>")
    s"$fileName#L$lineNo"
  }

  override def visitStatementsInBlock(b: Block, assignmentTarget: Option[Identifier] = None): Set[String] = {
    b.astChildren
      .map {
        case x: Call if x.name.startsWith(Operators.assignment) => visitAssignments(x.asInstanceOf[Assignment])
        case x: Call if x.name.startsWith("<operator>") && assignmentTarget.isDefined =>
          visitIdentifierAssignedToOperator(assignmentTarget.get, x, x.name)
        case x: Identifier if symbolTable.contains(x)       => symbolTable.get(x)
        case x: Identifier if globalSymbolTable.contains(x) => globalSymbolTable.get(x)
        case x: Call if symbolTable.contains(x)             => symbolTable.get(x)
        case x: Call if globalSymbolTable.contains(x)       => globalSymbolTable.get(x)
        case x: Call
            if x.argument.headOption.exists(arg => symbolTable.contains(arg) || globalSymbolTable.contains(arg)) =>
          setCallMethodFullNameFromBase(x)
        case x: Block            => visitStatementsInBlock(x)
        case x: Local            => symbolTable.get(x).union(globalSymbolTable.get(x))
        case _: ControlStructure => Set.empty[String]
        case x => logger.debug(s"Unhandled block element ${x.label}:${x.code} @ ${debugLocation(x)}"); Set.empty[String]
      }
      .lastOption
      .getOrElse(Set.empty[String])
  }

  override def setCallMethodFullNameFromBase(c: Call): Set[String] = {
    val recTypes = c.argument.headOption
      .map {
        case x: Call if x.typeFullName != "ANY" => Set(x.typeFullName)
        case x: Call =>
          cpg.method
            .fullNameExact(c.methodFullName)
            .flatMap(m => Try(m.methodReturn).toOption)
            .typeFullNameNot("ANY")
            .typeFullName
            .toSet match {
            case xs if xs.nonEmpty => xs
            case _ =>
              symbolTable
                .get(x)
                .union(globalSymbolTable.get(x))
                .map(t => Seq(t, XTypeRecovery.DummyReturnType).mkString(pathSep.toString))
          }
        case x => symbolTable.get(x).union(globalSymbolTable.get(x))
      }
      .getOrElse(Set.empty[String])
    val callTypes = recTypes.map(_.concat(s"$pathSep${c.name}"))
    symbolTable.append(c, callTypes)
  }

  override def associateInterproceduralTypes(
    i: Identifier,
    fieldFullName: String,
    fieldName: String,
    globalTypes: Set[String],
    baseTypes: Set[String]
  ): Set[String] = {
    if (globalTypes.nonEmpty) {
      // We have been able to resolve the type inter-procedurally
      associateTypes(i, globalTypes)
    } else if (baseTypes.nonEmpty) {
      lazy val existingMembers = cpg.typeDecl.fullNameExact(baseTypes.toSeq: _*).member.nameExact(fieldName)
      if (
        baseTypes.equals(symbolTable.get(LocalVar(fieldFullName)).union(globalSymbolTable.get(LocalVar(fieldFullName))))
      ) {
        associateTypes(i, baseTypes)
      } else if (existingMembers.isEmpty) {
        // If not available, use a dummy variable that can be useful for call matching
        associateTypes(i, baseTypes.map(t => XTypeRecovery.dummyMemberType(t, fieldName, pathSep)))
      } else {
        Set.empty
      }
    } else {
      // Assign dummy
      val dummyTypes = Set(
        XTypeRecovery.dummyMemberType(fieldFullName.stripSuffix(s"$pathSep$fieldName"), fieldName, pathSep)
      )
      associateTypes(i, dummyTypes)
    }
  }

  override def visitIdentifierAssignedToCallRetVal(i: Identifier, c: Call): Set[String] = {
    if (symbolTable.contains(c) || globalSymbolTable.contains(c)) {
      val callReturns = methodReturnValues(symbolTable.get(c).union(globalSymbolTable.get(c)).toSeq)
      associateTypes(i, callReturns)
    } else if (c.argument.exists(_.argumentIndex == 0)) {
      val callFullNames = (c.argument(0) match {
        case i: Identifier if symbolTable.contains(LocalVar(i.name))        => symbolTable.get(LocalVar(i.name))
        case i: Identifier if globalSymbolTable.contains(LocalVar(i.name))  => globalSymbolTable.get(LocalVar(i.name))
        case i: Identifier if symbolTable.contains(CallAlias(i.name))       => symbolTable.get(CallAlias(i.name))
        case i: Identifier if globalSymbolTable.contains(CallAlias(i.name)) => globalSymbolTable.get(CallAlias(i.name))
        case _                                                              => Set.empty
      }).map(_.concat(s"$pathSep${c.name}")).toSeq
      val callReturns = methodReturnValues(callFullNames)
      associateTypes(i, callReturns)
    } else {
      // Assign dummy value
      associateTypes(i, Set(s"${c.name}$pathSep${XTypeRecovery.DummyReturnType}"))
    }
  }

  override def visitCallAssignedToIdentifier(c: Call, i: Identifier): Set[String] = {
    val rhsTypes = symbolTable.get(i).union(globalSymbolTable.get(i))
    assignTypesToCall(c, rhsTypes)
  }

  override def getTypesFromCall(c: Call): Set[String] = c.name match {
    case Operators.fieldAccess =>
      symbolTable
        .get(LocalVar(getFieldName(c.asInstanceOf[FieldAccess])))
        .union(globalSymbolTable.get(LocalVar(getFieldName(c.asInstanceOf[FieldAccess]))))
    case _ if symbolTable.contains(c)       => methodReturnValues(symbolTable.get(c).toSeq)
    case _ if globalSymbolTable.contains(c) => methodReturnValues(globalSymbolTable.get(c).toSeq)
    case _ if c.argument.headOption.exists(arg => symbolTable.contains(arg) || globalSymbolTable.contains(arg)) =>
      setCallMethodFullNameFromBase(c)
      methodReturnValues(symbolTable.get(c).toSeq)
    case _ if isCallHeadArgumentAScopeResolutionAndIsLastArgumentInTable(c) =>
      setCallMethodFullNameFromBaseScopeResolution(c)
      methodReturnValues(symbolTable.get(c).toSeq)
    case Operators.indexAccess => getIndexAccessTypes(c)
    case n =>
      logger.debug(s"Unknown RHS call type '$n' @ ${debugLocation(c)}")
      Set.empty[String]
  }

  override def getIndexAccessTypes(ia: Call): Set[String] = indexAccessToCollectionVar(ia) match {
    case Some(cVar) if symbolTable.contains(cVar) =>
      symbolTable.get(cVar)
    case Some(cVar) if globalSymbolTable.contains(cVar) =>
      globalSymbolTable.get(cVar)
    case Some(cVar) if symbolTable.contains(LocalVar(cVar.identifier)) =>
      symbolTable.get(LocalVar(cVar.identifier)).map(x => s"$x$pathSep${XTypeRecovery.DummyIndexAccess}")
    case Some(cVar) if globalSymbolTable.contains(LocalVar(cVar.identifier)) =>
      globalSymbolTable.get(LocalVar(cVar.identifier)).map(x => s"$x$pathSep${XTypeRecovery.DummyIndexAccess}")
    case _ => Set.empty
  }

  override def visitCallAssignedToLiteral(c: Call, l: Literal): Set[String] = {
    if (c.name.equals(Operators.indexAccess)) {
      // For now, we will just handle this on a very basic level
      c.argumentOut.l match {
        case List(_: Identifier, _: Literal) =>
          indexAccessToCollectionVar(c).map(cv => symbolTable.append(cv, getLiteralType(l))).getOrElse(Set.empty)
        case List(_: Identifier, idx: Identifier) if symbolTable.contains(idx) =>
          // Imprecise but sound!
          indexAccessToCollectionVar(c).map(cv => symbolTable.append(cv, symbolTable.get(idx))).getOrElse(Set.empty)
        case List(_: Identifier, idx: Identifier) if globalSymbolTable.contains(idx) =>
          // Imprecise but sound!
          indexAccessToCollectionVar(c)
            .map(cv => symbolTable.append(cv, globalSymbolTable.get(idx)))
            .getOrElse(Set.empty)
        case List(i: Identifier, c: Call) =>
          // This is an expensive level of precision to support
          symbolTable.append(CollectionVar(i.name, "*"), getTypesFromCall(c))
        case List(c: Call, l: Literal) => assignTypesToCall(c, getLiteralType(l))
        case xs =>
          logger.debug(
            s"Unhandled index access point assigned to literal ${xs.map(x => (x.label, x.code)).mkString(",")} @ ${debugLocation(c)}"
          )
          Set.empty
      }
    } else if (c.name.equals(Operators.fieldAccess)) {
      val fa        = c.asInstanceOf[FieldAccess]
      val fieldName = getFieldName(fa)
      associateTypes(LocalVar(fieldName), fa, getLiteralType(l))
    } else {
      logger.warn(s"Unhandled call assigned to literal point ${c.name} @ ${debugLocation(c)}")
      Set.empty
    }
  }

  override def visitIdentifierAssignedToFieldLoad(i: Identifier, fa: FieldAccess): Set[String] = {
    val fieldName = getFieldName(fa)
    fa.argumentOut.l match {
      case ::(base: Identifier, ::(fi: FieldIdentifier, _)) if symbolTable.contains(LocalVar(base.name)) =>
        // Get field from global table if referenced as a variable
        val localTypes = symbolTable.get(LocalVar(base.name))
        associateInterproceduralTypes(i, base, fi, fieldName, localTypes)
      case ::(base: Identifier, ::(fi: FieldIdentifier, _)) if globalSymbolTable.contains(LocalVar(base.name)) =>
        // Get field from global table if referenced as a variable
        val localTypes = globalSymbolTable.get(LocalVar(base.name))
        associateInterproceduralTypes(i, base, fi, fieldName, localTypes)
      case ::(base: Identifier, ::(fi: FieldIdentifier, _)) if symbolTable.contains(LocalVar(fieldName)) =>
        val localTypes = symbolTable.get(LocalVar(fieldName))
        associateInterproceduralTypes(i, base, fi, fieldName, localTypes)
      case ::(base: Identifier, ::(fi: FieldIdentifier, _)) if globalSymbolTable.contains(LocalVar(fieldName)) =>
        val localTypes = globalSymbolTable.get(LocalVar(fieldName))
        associateInterproceduralTypes(i, base, fi, fieldName, localTypes)
      case ::(base: Identifier, ::(fi: FieldIdentifier, _)) =>
        val dummyTypes = Set(s"$fieldName$pathSep${XTypeRecovery.DummyReturnType}")
        associateInterproceduralTypes(i, base, fi, fieldName, dummyTypes)
      case ::(c: Call, ::(fi: FieldIdentifier, _)) if c.name.equals(Operators.fieldAccess) =>
        val baseName = getFieldName(c.asInstanceOf[FieldAccess])
        // Build type regardless of length
        // TODO: This is more prone to giving dummy values as it does not do global look-ups
        //  but this is okay for now
        val buf = mutable.ArrayBuffer.empty[String]
        for (segment <- baseName.split(pathSep) ++ Array(fi.canonicalName)) {
          val types =
            if (buf.isEmpty) symbolTable.get(LocalVar(segment)).union(globalSymbolTable.get(LocalVar(segment)))
            else
              buf
                .flatMap(t =>
                  symbolTable
                    .get(LocalVar(s"$t$pathSep$segment"))
                    .union(globalSymbolTable.get(LocalVar(s"$t$pathSep$segment")))
                )
                .toSet
          if (types.nonEmpty) {
            buf.clear()
            buf.addAll(types)
          } else {
            val bufCopy = Array.from(buf)
            buf.clear()
            bufCopy.foreach {
              case t if isConstructor(segment) => buf.addOne(s"$t$pathSep$segment")
              case t                           => buf.addOne(XTypeRecovery.dummyMemberType(t, segment, pathSep))
            }
          }
        }
        associateTypes(i, buf.toSet)
      case ::(call: Call, ::(fi: FieldIdentifier, _)) =>
        assignTypesToCall(
          call,
          Set(fieldName.stripSuffix(s"${XTypeRecovery.DummyMemberLoad}$pathSep${fi.canonicalName}"))
        )
      case _ =>
        logger.warn(s"Unable to assign identifier '${i.name}' to field load '$fieldName' @ ${debugLocation(i)}")
        Set.empty
    }
  }

  override def visitReturns(ret: Return): Unit = {
    val m = ret.method
    val existingTypes = mutable.HashSet.from(
      Try(
        (m.methodReturn.typeFullName +: m.methodReturn.dynamicTypeHintFullName)
          .filterNot(_ == "ANY")
      ).toOption.getOrElse(List())
    )

    @tailrec
    def extractTypes(xs: List[CfgNode]): Set[String] = xs match {
      case ::(head: Literal, Nil) if head.typeFullName != "ANY" =>
        Set(head.typeFullName)
      case ::(head: Call, Nil) if head.name == Operators.fieldAccess =>
        val fieldAccess = head.asInstanceOf[FieldAccess]
        val (sym, ts)   = getSymbolFromCall(fieldAccess)
        val cpgTypes = cpg.typeDecl
          .fullNameExact(ts.map(_.compUnitFullName).toSeq: _*)
          .member
          .nameExact(sym.identifier)
          .flatMap(m => m.typeFullName +: m.dynamicTypeHintFullName)
          .filterNot { x => x == "ANY" || x == "this" }
          .toSet
        if (cpgTypes.nonEmpty) cpgTypes
        else symbolTable.get(sym).union(globalSymbolTable.get(sym))
      case ::(head: Call, Nil) if symbolTable.contains(head) || globalSymbolTable.contains(head) =>
        val callPaths    = symbolTable.get(head).union(globalSymbolTable.get(head))
        val returnValues = methodReturnValues(callPaths.toSeq)
        if (returnValues.isEmpty)
          callPaths.map(c => s"$c$pathSep${XTypeRecovery.DummyReturnType}")
        else
          returnValues
      case ::(head: Call, Nil) if head.argumentOut.headOption.exists(symbolTable.contains) =>
        symbolTable
          .get(head.argumentOut.head)
          .map(t => Seq(t, head.name, XTypeRecovery.DummyReturnType).mkString(pathSep.toString))
      case ::(head: Call, Nil) if head.argumentOut.headOption.exists(globalSymbolTable.contains) =>
        globalSymbolTable
          .get(head.argumentOut.head)
          .map(t => Seq(t, head.name, XTypeRecovery.DummyReturnType).mkString(pathSep.toString))
      case ::(identifier: Identifier, Nil) if symbolTable.contains(identifier) =>
        symbolTable.get(identifier)
      case ::(identifier: Identifier, Nil) if globalSymbolTable.contains(identifier) =>
        globalSymbolTable.get(identifier)
      case ::(head: Call, Nil) =>
        extractTypes(head.argument.l)
      case _ => Set.empty
    }

    val returnTypes = extractTypes(ret.argumentOut.l)
    existingTypes.addAll(returnTypes)
    Try(builder.setNodeProperty(ret.method.methodReturn, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, existingTypes))
  }

  override def setTypeInformation(): Unit = {
    cu.ast
      .collect {
        case n: Local                                       => n
        case n: Call                                        => n
        case n: Expression                                  => n
        case n: MethodParameterIn if state.isFinalIteration => n
        case n: MethodReturn if state.isFinalIteration      => n
      }
      .foreach {
        case x: Local if symbolTable.contains(x)       => storeNodeTypeInfo(x, symbolTable.get(x).toSeq)
        case x: Local if globalSymbolTable.contains(x) => storeNodeTypeInfo(x, globalSymbolTable.get(x).toSeq)
        case x: MethodParameterIn                      => setTypeFromTypeHints(x)
        case x: MethodReturn                           => setTypeFromTypeHints(x)
        case x: Identifier if symbolTable.contains(x) || globalSymbolTable.contains(x) =>
          setTypeInformationForRecCall(x, x.inCall.headOption, x.inCall.argument.l)
        case x: Call if symbolTable.contains(x) =>
          val typs =
            if (state.config.enabledDummyTypes) symbolTable.get(x).toSeq
            else symbolTable.get(x).filterNot(XTypeRecovery.isDummyType).toSeq
          storeCallTypeInfo(x, typs)
        case x: Call if globalSymbolTable.contains(x) =>
          val typs =
            if (state.config.enabledDummyTypes) globalSymbolTable.get(x).toSeq
            else globalSymbolTable.get(x).filterNot(XTypeRecovery.isDummyType).toSeq
          storeCallTypeInfo(x, typs)
        case x: Identifier
            if (symbolTable
              .contains(CallAlias(x.name)) || globalSymbolTable.contains(CallAlias(x.name))) && x.inCall.nonEmpty =>
          setTypeInformationForRecCall(x, x.inCall.headOption, x.inCall.argument.l)
        case x: Call
            if x.argument.headOption.exists(arg => symbolTable.contains(arg) || globalSymbolTable.contains(arg)) =>
          setTypeInformationForRecCall(x, Option(x), x.argument.l)
        case x: Call
            if x.argument.headOption.exists(_.isCall) && !x.argument.isCall.head.name.startsWith("<operator>") =>
          val typs = getTypesFromCall(x.argument.isCall.head).map(_.concat(s".${x.name}")).toSeq
          storeCallTypeInfo(x, typs)
        case x: Call if isCallHeadArgumentAScopeResolutionAndIsLastArgumentInTable(x) =>
          setCallMethodFullNameFromBaseScopeResolution(x)
          val typs =
            if (state.config.enabledDummyTypes) symbolTable.get(x).toSeq
            else symbolTable.get(x).filterNot(XTypeRecovery.isDummyType).toSeq
          storeCallTypeInfo(x, typs)
        case _ =>
      }
    // Set types in an atomic way
    newTypesForMembers.foreach { case (m, ts) => storeDefaultTypeInfo(m, ts.toSeq) }
  }

  override protected def postSetTypeInformation(): Unit = {
    super.postSetTypeInformation()
    // method named `perform` can be called as `perform_async` to execute in parallel, below code links the call `perform_async` to `perform`
    cu.ast.isCall
      .nameExact("perform_async")
      .foreach(c =>
        storeCallTypeInfo(
          c,
          symbolTable.get(c).flatMap(fullName => List(fullName, fullName.stripSuffix("_async"))).toSeq
        )
      )
  }

  override def setTypeForDynamicDispatchCall(call: Call, i: Identifier): Unit = {
    val idHints   = symbolTable.get(i).union(globalSymbolTable.get(i))
    val callTypes = symbolTable.get(call).union(globalSymbolTable.get(call))
    persistType(i, idHints)
    if (callTypes.isEmpty && !call.name.startsWith("<operator>"))
      // For now, calls are treated as function pointers and thus the type should point to the method
      persistType(call, idHints.map(t => createCallFromIdentifierTypeFullName(t, call.name)))
    else {
      persistType(call, callTypes)
    }
  }

  override def setTypeForIdentifierAssignedToDefault(call: Call, i: Identifier): Unit = {
    val idHints = symbolTable.get(i).union(globalSymbolTable.get(i))
    persistType(i, idHints)
    persistType(call, idHints)
  }

  override def setTypeForIdentifierAssignedToCall(call: Call, i: Identifier, c: Call): Unit = {
    val idTypes =
      if (symbolTable.contains(i) || globalSymbolTable.contains(i)) symbolTable.get(i).union(globalSymbolTable.get(i))
      else symbolTable.get(CallAlias(i.name)).union(globalSymbolTable.get(CallAlias(i.name)))
    val callTypes = symbolTable.get(c).union(globalSymbolTable.get(c))
    persistType(call, callTypes)
    if (idTypes.nonEmpty || callTypes.nonEmpty) {
      if (idTypes.equals(callTypes))
        // Case 1.1: This is a function pointer or constructor
        persistType(i, callTypes)
      else
        // Case 1.2: This is the return value of the function
        persistType(i, idTypes)
    }
  }

  override def persistType(x: StoredNode, types: Set[String]): Unit = {
    val filteredTypes = if (state.config.enabledDummyTypes) types else types.filterNot(XTypeRecovery.isDummyType)
    if (filteredTypes.nonEmpty) {
      storeNodeTypeInfo(x, filteredTypes.toSeq)
      x match {
        case i: Identifier if symbolTable.contains(i) || globalSymbolTable.contains(i) =>
          if (isFieldUncached(i)) persistMemberType(i, filteredTypes)
          handlePotentialFunctionPointer(i, filteredTypes, i.name)
        case _ =>
      }
    }
  }

  private def setTypeInformationForRecCall(x: AstNode, n: Option[Call], ms: List[AstNode]): Unit = {
    (n, ms) match {
      // Case 1: 'call' is an assignment from some dynamic dispatch call
      case (Some(call: Call), ::(i: Identifier, ::(c: Call, _))) if call.name == Operators.assignment =>
        setTypeForIdentifierAssignedToCall(call, i, c)
      // Case 1: 'call' is an assignment from some other data structure
      case (Some(call: Call), ::(i: Identifier, _)) if call.name == Operators.assignment =>
        setTypeForIdentifierAssignedToDefault(call, i)
      // Case 2: 'i' is the receiver of 'call'
      case (Some(call: Call), ::(i: Identifier, _)) if call.name != Operators.fieldAccess =>
        setTypeForDynamicDispatchCall(call, i)
      // Case 3: 'i' is the receiver for a field access on member 'f'
      case (Some(fieldAccess: Call), ::(i: Identifier, ::(f: FieldIdentifier, _)))
          if fieldAccess.name == Operators.fieldAccess =>
        setTypeForFieldAccess(fieldAccess.asInstanceOf[FieldAccess], i, f)
      case _ =>
    }
    // Handle the node itself
    x match {
      case c: Call if c.name.startsWith("<operator") =>
      case c: Call if c.argument.headOption.exists(symbolTable.contains) =>
        c.argument.headOption match {
          case Some(callNode: Call) =>
            val types = symbolTable.get(c.argument.head)
            persistType(x, types.map(t => generateCallReturnFullName(t, c.name)))
          case _ => persistType(x, symbolTable.get(x))
        }
      case _ => persistType(x, symbolTable.get(x))
    }
  }

  private def storeNodeTypeInfo(storedNode: StoredNode, types: Seq[String]): Unit = {
    lazy val existingTypes = storedNode.getKnownTypes

    if (types.nonEmpty && types.toSet != existingTypes) {
      storedNode match {
        case m: Member =>
          // To avoid overwriting member updates, we store them elsewhere until the end
          newTypesForMembers.updateWith(m) {
            case Some(ts) => Some(ts ++ types)
            case None     => Some(types.toSet)
          }
        case i: Identifier                               => storeIdentifierTypeInfo(i, types)
        case l: Local                                    => storeLocalTypeInfo(l, types)
        case c: Call if !c.name.startsWith("<operator>") => storeCallTypeInfo(c, types)
        case _: Call                                     =>
        case n                                           => setTypes(n, types)
      }
    }
  }

  private def persistMemberType(i: Identifier, types: Set[String]): Unit = {
    getLocalMember(i) match {
      case Some(m) => storeNodeTypeInfo(m, types.toSeq)
      case None    =>
    }
  }
  override def storeCallTypeInfo(c: Call, types: Seq[String]): Unit =
    if (types.nonEmpty) super.storeCallTypeInfo(c, replaceConstructorType(types))

  override def storeDefaultTypeInfo(n: StoredNode, types: Seq[String]): Unit =
    if (types.toSet != n.getKnownTypes) super.storeDefaultTypeInfo(n, replaceConstructorType(types))

  /** For a given call fullname, append DummyReturnType and the call name, to form the call return value
    * @param fullName
    * @param callName
    * @return
    */
  private def generateCallReturnFullName(fullName: String, callName: String) =
    fullName.concat(s"$pathSep${XTypeRecovery.DummyReturnType}$pathSep$callName")

  /** Replace new.<returnType> and <init>.<returnType> with "", as these denote return by constructor
    * @param types
    * @return
    */
  private def replaceConstructorType(types: Seq[String]) = types.map(
    _.replaceAll(s".new.${XTypeRecovery.DummyReturnType}", "")
      .replaceAll(s".<init>.${XTypeRecovery.DummyReturnType}", "")
  )
}
