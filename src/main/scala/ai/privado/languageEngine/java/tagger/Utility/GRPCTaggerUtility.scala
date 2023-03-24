package ai.privado.languageEngine.java.tagger.Utility

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.model.{CatLevelOne, Language, NodeType, RuleInfo}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}

object GRPCTaggerUtility {

  // StreamObserverPattern to be used to identify type of field access for onNext & onCompleted methods
  private val StreamObserverPattern = "(?i)(.*)(StreamObserver)(.*)"

  def getGrpcEndpoints(cpg: Cpg): List[Method] = {
    val onCompleted = "onCompleted"
// Detecting `onCompleted` call by traversing function calls inside of all methods to narrow down gRPC services
    return cpg.method
      .where(
        _.call
          .name(onCompleted)
          .filter(_.argument.size == 1)
          .where(_.argument.typ.fullName(StreamObserverPattern))
      )
      .l
  }

  def getGrpcSinkCalls(cpg: Cpg, grpcEndpoints: List[Method]): List[Call] = {
    // `onNext` is always called inside of gRPC service methods
    // This `onNext` always has a StreamObserver in signature and takes only one argument
    val onNext        = "onNext"
    val stub          = "(?i)(.*)(stub)(.*)"
    var grpcSinkCalls = ListBuffer[Call]()

    grpcEndpoints.foreach(endpoint => {
      // Detecting gRPC API sink calls. These sink calls have the same name as function calls inside of onNext functions
      // To identify correct sinks, this filter makes sure sink call's parent is NOT `onNext`
      val sinks = cpg
        .call(endpoint.name)
        .whereNot(
          _.astParent.isCall
            .name(onNext)
            .filter(_.argument.size == 1)
            .where(_.argument.typ.fullName(StreamObserverPattern))
        )
        .l

      // Detecting `onNext` call inside of gRPC endpoint method
      // `onNext` takes on argument, this should be the call that satisfies gRPC/proto file contract
      // Get full type name of arguments that the server/endpoint will process
      val inCallArgTypes =
        endpoint.call
          .name(onNext)
          .filter(_.argument.size <= 2)
          .where(_.argument.typ.fullName(StreamObserverPattern))
          .argument
          .isCall
          .argument
          .isIdentifier
          .typeFullName
          .l

      val identifierArgTypes =
        endpoint.call
          .name(onNext)
          .filter(_.argument.size <= 2)
          .where(_.argument.typ.fullName(StreamObserverPattern))
          .argument
          .isIdentifier
          .typ
          .fullName
          .l

      sinks.foreach(sink => {
        // Get full type name of arguments going inside gRPC sink call
        val sinkArgTypes = sink.argument.isIdentifier.typeFullName.l

        sinkArgTypes.foreach(sinkArgType => {
          if (inCallArgTypes.contains(sinkArgType)) {
            grpcSinkCalls += sink
          }
          if (identifierArgTypes.contains(sinkArgType)) {
            grpcSinkCalls += sink
          }

          if (inCallArgTypes.size == 0) {
            if (sinkArgType.matches(stub)) {
              grpcSinkCalls += sink
            }
          }
          if (identifierArgTypes.size == 0) {
            if (sinkArgType.matches(stub)) {
              grpcSinkCalls += sink
            }
          }
        })

      })
    })

    return grpcSinkCalls.l
  }

  def getGrpcSinkRules(cpg: Cpg): RuleInfo = {
    val endpoints = getGrpcEndpoints(cpg)
    val methodFullNamesCombined =
      getGrpcSinkCalls(cpg, endpoints).map(sink => sink.methodFullName).mkString("(", "|", ")")
    return RuleInfo(
      "Sinks.RPC.gRPC.Call",
      "gRPC Call",
      "",
      Array[String]("grpc.io"),
      List[String](methodFullNamesCombined),
      false,
      "",
      HashMap[String, String](),
      NodeType.API,
      "",
      CatLevelOne.SINKS,
      "api",
      Language.JAVA,
      Array[String]()
    )
  }
}
