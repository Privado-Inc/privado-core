package ai.privado.tagger.sink

import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNode,
  CfgNode,
  NewFile,
  NewTag,
  Block,
  Literal,
  Call,
  Identifier,
  FieldIdentifier,
  Local
}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.utils.IOUtils
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer
import java.io.{PrintWriter, ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream}
import java.nio.file.Files

object SinkArgumentUtility {

  /** Populates argumentList for a GAPixel node. This function processes the given `node` to extract the arguments it
    * contains. Arguments can be literals, identifiers, or derived from assignment, call Nodes and spread operator
    * nodes.
    *
    * @param node
    *   The Call node representing the GAPixel.
    * @param cpg
    *   The Code Property Graph instance.
    * @return
    *   A list of key-value pairs representing the extracted arguments.
    */
  def addArgumentsForGAPixelNode(node: Call, cpg: Cpg): List[(String, String)] = {
    val keyValueStructures = ListBuffer.empty[(String, String)]

    val assignmentNodes     = node.argument.isBlock.astChildren.isCall.name(Operators.assignment).l
    val spreadOperatorNodes = node.argument.isBlock.astChildren.isCall.name("<operator>.spread").l
    val identifierNodes     = node.argument.isIdentifier.l
    val literalNodes        = node.argument.isLiteral.l

    literalNodes.foreach { l =>
      keyValueStructures += ((l.code, l.code))
    }
    identifierNodes.foreach { i =>
      processIdentifierNode(i, "", cpg, keyValueStructures)
    }

    processAssignmentNodes(assignmentNodes, "", cpg, keyValueStructures)
    processSpreadOperatorNodes(spreadOperatorNodes, "", cpg, keyValueStructures)

    keyValueStructures.toList
  }

  /** Processes an Identifier node to extract its type information and member names. If typeDecl found for identifier,
    * check for members related to it.
    */
  private def processIdentifierNode(
    n: Identifier,
    topLeftKey: String,
    cpg: Cpg,
    keyValueStructures: ListBuffer[(String, String)]
  ): Unit = {
    def updateLeftKey(code: String): String = {
      if (topLeftKey.nonEmpty)
        topLeftKey + "." + code
      else
        code
    }

    val identifierTypeFullName = n.typeFullName
    val quotedTypeFullNameStr  = Regex.quote(identifierTypeFullName)

    val memberList = cpg.typeDecl(quotedTypeFullNameStr).member.name.l

    val pattern = """\b([a-zA-Z_]\w*):""".r

    val matches = pattern.findAllMatchIn(identifierTypeFullName)
    if (!n.name.matches("(_tmp_|this|globalThis|eventEmitter|analyticsService|segmentPayloads).*")) {
      if (memberList.nonEmpty) {
        memberList.foreach((key) => {
          keyValueStructures += ((updateLeftKey(key), key))
        })

      } else if (matches.nonEmpty) {
        val keys = matches.map(_.group(1)).toList
        keys.foreach((key) => {
          keyValueStructures += ((updateLeftKey(n.code + "." + key), key))
        })
      } else {
        keyValueStructures += ((updateLeftKey(n.code), identifierTypeFullName))
      }
    }
  }

  /** Processes assignment nodes to extract key-value pairs. Examples: { x: "test" } Here "test" is getting assigned to
    * x which connected via assignment Node
    */
  private def processAssignmentNodes(
    assignmentNodes: List[Call],
    topLeftKey: String,
    cpg: Cpg,
    keyValueStructures: ListBuffer[(String, String)]
  ): Unit = {
    assignmentNodes.foreach { keyVal =>
      val (leftChild, rightChild) = extractKeyValPair(keyVal.astChildren.l, Some(topLeftKey), cpg, keyValueStructures)
      for {
        left  <- leftChild
        right <- rightChild
      } {
        if (right.nonEmpty) {
          keyValueStructures += ((left, right))
        }
      }
    }
  }

  /** Processes spread operator nodes to extract key-value pairs. Handles examples like below { ...product }
    * [...products, "p4"] [...a.test()]
    */
  private def processSpreadOperatorNodes(
    spreadOperatorNodes: List[Call],
    topLeftKey: String,
    cpg: Cpg,
    keyValueStructures: ListBuffer[(String, String)]
  ): Unit = {
    spreadOperatorNodes.foreach { keyVal =>
      val callNodes       = keyVal.astChildren.isCall.l
      val childCallNodes  = callNodes.headOption.map(_.astChildren.l).getOrElse(List.empty)
      val identifierNodes = keyVal.astChildren.isIdentifier.lastOption.l
      callNodes.isCall.foreach { callN =>
        handlePayloadCallNode(callN, topLeftKey, cpg, keyValueStructures)
        handlePICKMethodCallNode(callN, topLeftKey, keyValueStructures)
      }
      (childCallNodes.collect { case n: Identifier => n } ++ identifierNodes.collect { case n: Identifier => n })
        .foreach(processIdentifierNode(_, topLeftKey, cpg, keyValueStructures))
    }
  }

  /** Gets the left key from the given AstNode This is to get nested structure in key name { a: { b: { c: "test" }} } ->
    * "a.b.c"
    * @return
    *   An option containing the left key as a string, or None if not found.
    */
  private def getLeftKey(
    leftKey: AstNode,
    topLeftKey: Option[String],
    cpg: Cpg,
    keyValueStructures: ListBuffer[(String, String)]
  ): Option[String] = {
    val res = Some(leftKey)
      .collect {
        case c: Call    => handleCallNode(c, topLeftKey, cpg, keyValueStructures).astChildren.isFieldIdentifier.head
        case l: Literal => l
      }
      .map(_.code)
    if (topLeftKey.getOrElse("").nonEmpty)
      Some(topLeftKey.getOrElse("") + "." + res.getOrElse(""))
    else
      res
  }

  /** Extracts the left and right child nodes of a key-value pair. This function has generic logic to process all kinds
    * of possible nodes { a: "test" | test | test() | x.test | { ... } }
    * @return
    *   A tuple containing the left and right child nodes as options.
    */
  private def extractKeyValPair(
    objKeyVal: List[AstNode],
    topLeftKey: Option[String],
    cpg: Cpg,
    keyValueStructures: ListBuffer[(String, String)]
  ): (Option[String], Option[String]) = {
    objKeyVal match {
      case leftKey :: rightVal :: _ =>
        (
          getLeftKey(leftKey, topLeftKey, cpg, keyValueStructures),
          Some(rightVal).collect {
            case l: Literal         => l.code
            case l: Identifier      => l.code
            case l: FieldIdentifier => l.code
            case l: Local           => l.code
            case l: Call            => handleCallNode(l, topLeftKey, cpg, keyValueStructures).code
            case l: Block =>
              handleBlockNode(l, getLeftKey(leftKey, topLeftKey, cpg, keyValueStructures), cpg, keyValueStructures)
          }
        )
      case _ =>
        (None, None)
    }
  }

  /** For payload sink node, maps it with available googlePayload event in obj Identifies the eventName & tries to map
    * with blockNode key If matched further process the implemented method against it.
    */
  private def handlePayloadCallNode(
    callNode: Call,
    topLeftKey: String,
    cpg: Cpg,
    keyValueStructures: ListBuffer[(String, String)]
  ): Unit = {
    if (callNode.name.equals("payload")) {
      val gpEventKey =
        callNode.astChildren.isCall.astChildren.isCall.astChildren.isCall.astChildren.isFieldIdentifier.code.headOption
          .getOrElse("")
      val blockNode = cpg.call.code(".*tmp.*" + gpEventKey + " =.*").astChildren.isBlock.l

      if (blockNode.nonEmpty) {
        val internalBlockNode = blockNode.head.astChildren.isCall
          .code(".*payload.*")
          .astChildren
          .isMethodRef
          .referencedMethod
          .astChildren
          .isBlock
          .l
        val returnBlockNode = internalBlockNode.astChildren.isReturn.astChildren.isBlock.l
        handleBlockNode(blockNode.head, Some(topLeftKey), cpg, keyValueStructures)
        handleBlockNode(returnBlockNode.head, Some(topLeftKey), cpg, keyValueStructures)
      }
    }
  }

  /** Check for call node, also have special conditions test() Handling `payload` method for GTM Handling lodash.pick
    * method
    */
  private def handleCallNode(
    callNode: Call,
    topLeftKey: Option[String],
    cpg: Cpg,
    keyValueStructures: ListBuffer[(String, String)]
  ): AstNode = {
    // Handling `payload` method for GTM differently
    handlePayloadCallNode(callNode, topLeftKey.getOrElse("") + ".payload", cpg, keyValueStructures)
    handlePICKMethodCallNode(callNode, topLeftKey.getOrElse(""), keyValueStructures)
    // TODO: Handle Json.stringify call node
    // TODO: Handle scriptSafe call node

    if (callNode.methodFullName.equals("__ecma.Array:")) {
      val blockNodes = callNode.astChildren.isBlock.l
      blockNodes.foreach { bNode =>
        handleBlockNode(bNode, Some(topLeftKey.getOrElse("") + ".[]"), cpg, keyValueStructures)
      }
    }

    callNode
  }

  /** Identifies the nested block structure & identify all fields within it { a: "test" | test | test() | x.test | { ...
    * } }
    */
  private def handleBlockNode(
    blockNode: Block,
    topLeftKey: Option[String],
    cpg: Cpg,
    keyValueStructures: ListBuffer[(String, String)]
  ): String = {
    val assignmentNodes     = blockNode.astChildren.isCall.name(Operators.assignment).l
    val spreadOperatorNodes = blockNode.astChildren.isCall.name("<operator>.spread").l
    val childBlockNodes     = blockNode.astChildren.isBlock.l
    val arrayCallNodes      = blockNode.astChildren.isCall.methodFullName("__ecma.Array:").astChildren.isBlock.l

    processAssignmentNodes(assignmentNodes, topLeftKey.getOrElse(""), cpg, keyValueStructures)
    processSpreadOperatorNodes(spreadOperatorNodes, topLeftKey.getOrElse(""), cpg, keyValueStructures)
    childBlockNodes.foreach { bNode =>
      handleBlockNode(bNode, topLeftKey, cpg, keyValueStructures)
    }

    arrayCallNodes.foreach { bNode =>
      handleBlockNode(bNode, Some(topLeftKey.getOrElse("") + ".[]"), cpg, keyValueStructures)
    }

    ""
  }

  /** Very specific to lodash pick method pick(product, ['id', 'name', 'ext_id', 'ic_product_id']) here from product obj
    * we are picking fields mentioned in list we create key-val like `product[].id: "id"`
    */
  private def handlePICKMethodCallNode(
    callNode: Call,
    topLeftKey: String,
    keyValueStructures: ListBuffer[(String, String)]
  ): Unit = {
    if (callNode.name.equals("pick")) {
      var concatKey = ""
      val keys      = callNode.astChildren.isBlock.astChildren.isCall.astChildren.isLiteral.code.l
      val objName   = callNode.astChildren.isIdentifier.filter(i => !i.name.matches("this|pick")).name.l

      if (objName.nonEmpty) {
        concatKey = objName.head
      }

      keys.foreach { key =>
        keyValueStructures += ((topLeftKey + "." + key, concatKey + "." + key))
      }
    }
  }

  def serializedArgumentString(originalList: List[(String, String)]): String = {
    val byteArrayOutputStream = new ByteArrayOutputStream()
    val objectOutputStream    = new ObjectOutputStream(byteArrayOutputStream)
    objectOutputStream.writeObject(originalList.toMap)
    objectOutputStream.close()
    byteArrayOutputStream.toString("ISO-8859-1")
  }

  def deserializedArgumentString(serializedString: String): Map[String, String] = {
    if (serializedString.isEmpty) {
      Map.empty[String, String]
    } else {
      val byteArrayInputStream = new ByteArrayInputStream(serializedString.getBytes("ISO-8859-1"))
      val objectInputStream    = new ObjectInputStream(byteArrayInputStream)
      try {
        val result = objectInputStream.readObject().asInstanceOf[Map[String, String]]
        result
      } catch {
        case ex: Exception =>
          Map.empty[String, String]
      } finally {
        objectInputStream.close()
      }
    }
  }
}
