package ai.privado.languageEngine.ruby.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.joern.x2cpg.passes.frontend.*
import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromNodeExt
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.Defines.{ConstructorMethodName, DynamicCallUnknownFullName}
import io.joern.x2cpg.Defines as XDefines
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, Operators, PropertyNames}
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.{Assignment, FieldAccess}
import overflowdb.BatchedUpdate.DiffGraphBuilder

import scala.annotation.tailrec
import scala.collection.{Seq, mutable}

class PrivadoRubyTypeRecoveryPass(
  cpg: Cpg,
  globalSymbolTable: SymbolTable[LocalKey],
  config: XTypeRecoveryConfig = XTypeRecoveryConfig()
) extends XTypeRecoveryPass[File](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState): XTypeRecovery[File] =
    new RubyTypeRecovery(cpg, globalSymbolTable, state)
}

private class RubyTypeRecovery(cpg: Cpg, globalSymbolTable: SymbolTable[LocalKey], state: XTypeRecoveryState)
    extends XTypeRecovery[File](cpg, state) {

  override def compilationUnit: Traversal[File] = cpg.file.iterator

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] = {
    val newConfig = state.config.copy(enabledDummyTypes = state.isFinalIteration && state.config.enabledDummyTypes)
    new RecoverForRubyFile(cpg, globalSymbolTable, unit, builder, state.copy(config = newConfig))
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

  /** A heuristic method to determine if a call is a constructor or not.
    */
  override protected def isConstructor(c: Call): Boolean = {
    isConstructor(c.name) && c.code.charAt(0).isUpper
  }

  /** A heuristic method to determine if a call name is a constructor or not.
    */
  override protected def isConstructor(name: String): Boolean =
    !name.isBlank && name.equals("new")

  override def visitImport(i: Import): Unit = for {
    resolvedImport <- i.call.tag
    alias          <- i.importedAs
  } {
    import io.joern.x2cpg.passes.frontend.ImportsPass._
    ResolvedImport.tagToResolvedImport(resolvedImport).foreach {
      case ResolvedTypeDecl(fullName, _) =>
        symbolTable.append(LocalVar(fullName.split("\\.").lastOption.getOrElse(alias)), fullName)
      case _ => super.visitImport(i)
    }
  }
  override def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {

    def isMatching(cName: String, code: String) = {
      val cNameList = cName.split(":program").last.split("\\.").filterNot(_.isEmpty)
      val codeList  = code.split("\\(").head.split("[:.]").filterNot(_.isEmpty)
      cNameList sameElements codeList
    }

    val constructorPaths = symbolTable.get(c).filter(isMatching(_, c.code)).map(_.stripSuffix(s"${pathSep}new"))
    associateTypes(i, constructorPaths)
  }

  /*
  override def methodReturnValues(methodFullNames: Seq[String]): Set[String] = {
    // Check if we have a corresponding member to resolve type
    val memberTypes = methodFullNames.flatMap { fullName =>
      val memberName = fullName.split("\\.").lastOption
      if (memberName.isDefined) {
        val typeDeclFullName = fullName.stripSuffix(s".${memberName.get}")
        cpg.typeDecl.fullName(typeDeclFullName).member.nameExact(memberName.get).typeFullName.l
      } else
        List.empty
    }.toSet
    if (memberTypes.nonEmpty) memberTypes else super.methodReturnValues(methodFullNames)
  }
   */

  override def visitIdentifierAssignedToCall(i: Identifier, c: Call): Set[String] = {
    if (c.name.startsWith("<operator>")) {
      visitIdentifierAssignedToOperator(i, c, c.name)
    } else if ((symbolTable.contains(c) || globalSymbolTable.contains(c)) && isConstructor(c)) {
      visitIdentifierAssignedToConstructor(i, c)
    } else if (symbolTable.contains(c) || globalSymbolTable.contains(c)) {
      visitIdentifierAssignedToCallRetVal(i, c)
    } else if (c.argument.headOption.exists(arg => symbolTable.contains(arg) || globalSymbolTable.contains(arg))) {
      setCallMethodFullNameFromBase(c)
      // Repeat this method now that the call has a type
      visitIdentifierAssignedToCall(i, c)
    } else if (
      c.argument.headOption
        .exists(_.isCall) && c.argument.head
        .asInstanceOf[Call]
        .name
        .equals("<operator>.scopeResolution") && c.argument.head
        .asInstanceOf[Call]
        .argument
        .lastOption
        .exists(arg => symbolTable.contains(arg) || globalSymbolTable.contains(arg))
    ) {
      setCallMethodFullNameFromBaseScopeResolution(c)
      // Repeat this method now that the call has a type
      visitIdentifierAssignedToCall(i, c)
    } else {
      // We can try obtain a return type for this call
      visitIdentifierAssignedToCallRetVal(i, c)
    }
  }

  protected def setCallMethodFullNameFromBaseScopeResolution(c: Call): Set[String] = {
    val recTypes = c.argument.headOption
      .map {
        case x: Call if x.name.equals("<operator>.scopeResolution") =>
          x.argument.lastOption
            .map(i => symbolTable.get(i).union(globalSymbolTable.get(i)))
            .getOrElse(Set.empty[String])
      }
      .getOrElse(Set.empty[String])
    val callTypes = recTypes.map(_.concat(s"$pathSep${c.name}"))
    symbolTable.append(c, callTypes)
  }

  private def debugLocation(n: AstNode): String = {
    val fileName = n.file.name.headOption.getOrElse("<unknown>").stripPrefix(codeRoot)
    val lineNo   = n.lineNumber.getOrElse("<unknown>")
    s"$fileName#L$lineNo"
  }

  override def visitStatementsInBlock(b: Block, assignmentTarget: Option[Identifier] = None): Set[String] = {
    b.astChildren
      .map {
        case x: Call if x.name.startsWith(Operators.assignment) => visitAssignments(new Assignment(x))
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
          cpg.method.fullNameExact(c.methodFullName).methodReturn.typeFullNameNot("ANY").typeFullName.toSet match {
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
        .get(LocalVar(getFieldName(new FieldAccess(c))))
        .union(globalSymbolTable.get(LocalVar(getFieldName(new FieldAccess(c)))))
    case _ if symbolTable.contains(c)       => methodReturnValues(symbolTable.get(c).toSeq)
    case _ if globalSymbolTable.contains(c) => globalSymbolTable.get(c)
    case Operators.indexAccess              => getIndexAccessTypes(c)
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
      val fa        = new FieldAccess(c)
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
        val baseName = getFieldName(new FieldAccess(c))
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
      (m.methodReturn.typeFullName +: m.methodReturn.dynamicTypeHintFullName)
        .filterNot(_ == "ANY")
    )

    @tailrec
    def extractTypes(xs: List[CfgNode]): Set[String] = xs match {
      case ::(head: Literal, Nil) if head.typeFullName != "ANY" =>
        Set(head.typeFullName)
      case ::(head: Call, Nil) if head.name == Operators.fieldAccess =>
        val fieldAccess = new FieldAccess(head)
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
    builder.setNodeProperty(ret.method.methodReturn, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, existingTypes)
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
          storeCallTypeInfoPrivado(x, typs)
        case x: Call if globalSymbolTable.contains(x) =>
          val typs =
            if (state.config.enabledDummyTypes) globalSymbolTable.get(x).toSeq
            else globalSymbolTable.get(x).filterNot(XTypeRecovery.isDummyType).toSeq
          storeCallTypeInfoPrivado(x, typs)
        case x: Identifier
            if (symbolTable
              .contains(CallAlias(x.name)) || globalSymbolTable.contains(CallAlias(x.name))) && x.inCall.nonEmpty =>
          setTypeInformationForRecCall(x, x.inCall.headOption, x.inCall.argument.l)
        case x: Call if x.argument.headOption.exists(symbolTable.contains) =>
          setTypeInformationForRecCall(x, Option(x), x.argument.l)
        case x: Call if x.argument.headOption.exists(globalSymbolTable.contains) =>
          setTypeInformationForRecCall(x, Option(x), x.argument.l)
        case _ =>
      }
    // Set types in an atomic way
    newTypesForMembers.foreach { case (m, ts) => storeDefaultTypeInfoPrivado(m, ts.toSeq) }
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
          if (isField(i)) persistMemberType(i, filteredTypes)
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
        setTypeForFieldAccess(new FieldAccess(fieldAccess), i, f)
      case _ =>
    }
    // Handle the node itself
    x match {
      case c: Call if c.name.startsWith("<operator") =>
      case _                                         => persistType(x, symbolTable.get(x))
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
        case i: Identifier                               => storeIdentifierTypeInfoPrivado(i, types)
        case l: Local                                    => storeLocalTypeInfoPrivado(l, types)
        case c: Call if !c.name.startsWith("<operator>") => storeCallTypeInfoPrivado(c, types)
        case _: Call                                     =>
        case n =>
          state.changesWereMade.compareAndSet(false, true)
          setTypesPrivado(n, types)
      }
    }
  }

  def storeIdentifierTypeInfoPrivado(i: Identifier, types: Seq[String]): Unit =
    storeDefaultTypeInfoPrivado(i, types)

  /** Allows one to modify the types assigned to nodes otherwise.
    */
  def storeDefaultTypeInfoPrivado(n: StoredNode, types: Seq[String]): Unit =
    if (types.toSet != n.getKnownTypes) {
      state.changesWereMade.compareAndSet(false, true)
      setTypesPrivado(n, (n.property(PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, Seq.empty) ++ types).distinct)
    }

  def setTypesPrivado(n: StoredNode, types: Seq[String]): Unit =
    if (types.size == 1) builder.setNodeProperty(n, PropertyNames.TYPE_FULL_NAME, types.head)
    else builder.setNodeProperty(n, PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME, types)

  /** Allows one to modify the types assigned to locals.
    */
  def storeLocalTypeInfoPrivado(l: Local, types: Seq[String]): Unit = {
    storeDefaultTypeInfoPrivado(
      l,
      if (state.config.enabledDummyTypes) types else types.filterNot(XTypeRecovery.isDummyType)
    )
  }

  def storeCallTypeInfoPrivado(c: Call, types: Seq[String]): Unit =
    if (types.nonEmpty) {
      state.changesWereMade.compareAndSet(false, true)
      builder.setNodeProperty(
        c,
        PropertyNames.DYNAMIC_TYPE_HINT_FULL_NAME,
        (c.dynamicTypeHintFullName ++ types).distinct
      )
    }
  private def persistMemberType(i: Identifier, types: Set[String]): Unit = {
    getLocalMember(i) match {
      case Some(m) => storeNodeTypeInfo(m, types.toSeq)
      case None    =>
    }
  }

  override protected def handlePotentialFunctionPointer(
    funcPtr: Expression,
    baseTypes: Set[String],
    funcName: String,
    baseName: Option[String] = None
  ): Unit = {
    // Sometimes the function identifier is an argument to the call itself as a "base". In this case we don't need
    // a method ref. This happens in jssrc2cpg
    if (!funcPtr.astParent.iterator.collectAll[Call].exists(_.name == funcName)) {
      baseTypes
        .map(t => if (t.endsWith(funcName)) t else s"$t$pathSep$funcName")
        .flatMap(cpg.method.fullNameExact)
        .filterNot(m => addedNodes.contains(s"${funcPtr.id()}${NodeTypes.METHOD_REF}$pathSep${m.fullName}"))
        .map(m => m -> createMethodRef(baseName, funcName, m.fullName, funcPtr.lineNumber, funcPtr.columnNumber))
        .foreach { case (m, mRef) =>
          funcPtr.astParent
            .filterNot(_.astChildren.isMethodRef.exists(_.methodFullName == mRef.methodFullName))
            .foreach { inCall =>
              state.changesWereMade.compareAndSet(false, true)
              integrateMethodRef(funcPtr, m, mRef, inCall)
            }
        }
    }
  }

  private def integrateMethodRef(funcPtr: Expression, m: Method, mRef: NewMethodRef, inCall: AstNode) = {
    builder.addNode(mRef)
    builder.addEdge(mRef, m, EdgeTypes.REF)
    builder.addEdge(inCall, mRef, EdgeTypes.AST)
    builder.addEdge(funcPtr.method, mRef, EdgeTypes.CONTAINS)
    inCall match {
      case x: Call =>
        builder.addEdge(x, mRef, EdgeTypes.ARGUMENT)
        mRef.argumentIndex(x.argumentOut.size + 1)
      case x =>
        mRef.argumentIndex(x.astChildren.size + 1)
    }
    addedNodes.add(s"${funcPtr.id()}${NodeTypes.METHOD_REF}$pathSep${mRef.methodFullName}")
  }
  private def createMethodRef(
    baseName: Option[String],
    funcName: String,
    methodFullName: String,
    lineNo: Option[Integer],
    columnNo: Option[Integer]
  ): NewMethodRef =
    NewMethodRef()
      .code(s"${baseName.map(_.appended(pathSep)).getOrElse("")}$funcName")
      .methodFullName(methodFullName)
      .lineNumber(lineNo)
      .columnNumber(columnNo)

}
