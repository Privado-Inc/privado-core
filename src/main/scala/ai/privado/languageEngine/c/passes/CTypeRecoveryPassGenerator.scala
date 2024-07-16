package ai.privado.languageEngine.c.passes

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.{
  CallAlias,
  LocalVar,
  RecoverForXCompilationUnit,
  XTypeRecovery,
  XTypeRecoveryConfig,
  XTypeRecoveryPassGenerator,
  XTypeRecoveryState
}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import overflowdb.traversal.Traversal
import io.shiftleft.semanticcpg.language.*
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.semanticcpg.language.importresolver.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.{Assignment, FieldAccess}
import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromIteratorExt
import io.joern.x2cpg.passes.frontend.XTypeRecovery.AllNodeTypesFromNodeExt
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}

class CTypeRecoveryPassGenerator(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
    extends XTypeRecoveryPassGenerator[File](cpg, config) {
  override protected def generateRecoveryPass(state: XTypeRecoveryState, iteration: Int): XTypeRecovery[File] =
    new CTypeRecovery(cpg, state, iteration)
}

private class CTypeRecovery(cpg: Cpg, state: XTypeRecoveryState, iteration: Int)
    extends XTypeRecovery[File](cpg, state, iteration) {
  override def compilationUnits: Traversal[File] = cpg.file.iterator

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] = {
    new RecoverForCFile(cpg, unit, builder, state)
  }
}

private class RecoverForCFile(cpg: Cpg, cu: File, builder: DiffGraphBuilder, state: XTypeRecoveryState)
    extends RecoverForXCompilationUnit[File](cpg, cu, builder, state) {

  /** A heuristic method to determine if a call is a constructor or not.
    */
  override protected def isConstructor(c: Call): Boolean = {
    isConstructor(c.name)
  }

  /** A heuristic method to determine if a call name is a constructor or not.
    */
  override protected def isConstructor(name: String): Boolean =
    !name.isBlank && (name.equals("new") || name.equals("<init>"))

  override protected def importNodes: Iterator[Import] = cu match {
    case x: File => cpg.imports.where(_.file.nameExact(s"${x.name}"))
    case _       => super.importNodes
  }

  override protected def visitImport(i: Import): Unit = for {
    resolvedImport <- i.tag
    alias          <- i.importedAs
  } {
    EvaluatedImport.tagToEvaluatedImport(resolvedImport).foreach {
      case ResolvedMethod(fullName, alias, receiver, _) =>
        symbolTable.append(CallAlias(alias, receiver), fullName)
      case ResolvedTypeDecl(fullName, _) =>
        symbolTable.append(LocalVar(alias), fullName)
      case ResolvedMember(basePath, memberName, _) =>
        val matchingIdentifiers = cpg.method.fullNameExact(basePath).local
        val matchingMembers     = cpg.typeDecl.fullNameExact(basePath).member
        val memberTypes = (matchingMembers ++ matchingIdentifiers)
          .nameExact(memberName)
          .getKnownTypes
        symbolTable.append(LocalVar(alias), memberTypes)
      case UnknownMethod(fullName, alias, receiver, _) =>
        symbolTable.append(CallAlias(alias, receiver), fullName)
      case UnknownTypeDecl(fullName, _) =>
        symbolTable.append(LocalVar(alias), fullName)
      case UnknownImport(path, _) =>
        symbolTable.append(CallAlias(alias), path)
        symbolTable.append(LocalVar(alias), path)
    }
  }

  override protected def hasTypes(node: AstNode): Boolean = node match {
    case x: Call if !x.methodFullName.startsWith("<operator>") =>
      !x.methodFullName.toLowerCase().matches("(<unknownfullname>|any)") && !x.methodFullName.equals(x.name)
    case x => x.getKnownTypes.nonEmpty
  }

  override protected def setCallMethodFullNameFromBase(c: Call): Set[String] = {
    val recTypes = c.argument.headOption
      .map {
        case ifa: Call if ifa.name.equals("<operator>.indirectFieldAccess") =>
          getTypeFromArgument(ifa.argument.headOption, c)
        case x => getTypeFromArgument(Some(x), c)
      }
      .getOrElse(Set.empty[String])
    val callTypes = recTypes.map(_.stripSuffix("*").concat(s"$pathSep${c.name}"))
    symbolTable.append(c, callTypes)
  }

  private def getTypeFromArgument(headArgument: Option[Expression], c: Call): Set[String] = {
    headArgument
      .map {
        case x: Call if x.typeFullName != "ANY" && x.typeFullName != "<empty>" =>
          Set(x.typeFullName)
        case x: Call =>
          val returns                 = cpg.method.fullNameExact(c.methodFullName).methodReturn.typeFullNameNot("ANY")
          val returnWithPossibleTypes = cpg.method.fullNameExact(c.methodFullName).methodReturn.where(_.possibleTypes)
          val fullNames               = returns.typeFullName ++ returnWithPossibleTypes.possibleTypes
          fullNames.toSet match {
            case xs if xs.nonEmpty => xs
            case _ =>
              val returns = cpg.method.fullNameExact(x.methodFullName).methodReturn.typeFullNameNot("ANY")
              val returnWithPossibleTypes =
                cpg.method.fullNameExact(x.methodFullName).methodReturn.where(_.possibleTypes)
              val fullNames = returns.typeFullName ++ returnWithPossibleTypes.possibleTypes
              fullNames.toSet match {
                case xs if xs.nonEmpty => xs
                case _ => symbolTable.get(x).map(t => Seq(t, XTypeRecovery.DummyReturnType).mkString(pathSep))
              }
          }
        case x =>
          symbolTable.get(x)
      }
      .getOrElse(Set.empty[String])
  }

  override protected def setTypeInformation(): Unit = {
    cu.ast
      .collect {
        case n: Local                                       => n
        case n: Call                                        => n
        case n: Expression                                  => n
        case n: MethodParameterIn if state.isFinalIteration => n
        case n: MethodReturn if state.isFinalIteration      => n
      }
      .foreach {
        case x: Local if symbolTable.contains(x) => storeNodeTypeInfo(x, symbolTable.get(x).toSeq)
        case x: MethodParameterIn                => setTypeFromTypeHints(x)
        case x: MethodReturn =>
          setTypeFromTypeHints(x)
        case x: Identifier if symbolTable.contains(x) =>
          setTypeInformationForRecCall(x, x.inCall.headOption, x.inCall.argument.l)
        case x: Call if symbolTable.contains(x) =>
          val typs =
            if (state.enableDummyTypesForThisIteration) symbolTable.get(x).toSeq
            else symbolTable.get(x).filterNot(XTypeRecovery.isDummyType).toSeq
          storeCallTypeInfo(x, typs)
        case x: Identifier if symbolTable.contains(CallAlias(x.name)) && x.inCall.nonEmpty =>
          setTypeInformationForRecCall(x, x.inCall.headOption, x.inCall.argument.l)
        case x: Call if x.argument.headOption.isCall.exists(_.name.equals("<operator>.indirectFieldAccess")) =>
          setCallMethodFullNameFromBase(x)
          val typs =
            if (state.enableDummyTypesForThisIteration) symbolTable.get(x).toSeq
            else symbolTable.get(x).filterNot(XTypeRecovery.isDummyType).toSeq
          storeCallTypeInfo(x, typs)
        case x: Call if x.argument.headOption.exists(symbolTable.contains) =>
          setTypeInformationForRecCall(x, Option(x), x.argument.l)
        case _ =>
      }
    // Set types in an atomic way
    newTypesForMembers.foreach { case (m, ts) => storeDefaultTypeInfo(m, ts.toSeq) }
  }

  private def storeNodeTypeInfo(storedNode: StoredNode, types: Seq[String]): Unit = {
    lazy val existingTypes = storedNode.getKnownTypes

    val hasUnknownTypeFullName = storedNode
      .property(PropertyNames.TYPE_FULL_NAME, Defines.Any)
      .matches(XTypeRecovery.unknownTypePattern.pattern.pattern())

    if (types.nonEmpty && (hasUnknownTypeFullName || types.toSet != existingTypes)) {
      storedNode match {
        case m: Member =>
          // To avoid overwriting member updates, we store them elsewhere until the end
          newTypesForMembers.updateWith(m) {
            case Some(ts) => Option(ts ++ types)
            case None     => Option(types.toSet)
          }
        case i: Identifier                               => storeIdentifierTypeInfo(i, types)
        case l: Local                                    => storeLocalTypeInfo(l, types)
        case c: Call if !c.name.startsWith("<operator>") => storeCallTypeInfo(c, types)
        case _: Call                                     =>
        case n =>
          setTypes(n, types)
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
      case _                                         => persistType(x, symbolTable.get(x))
    }
  }
}
