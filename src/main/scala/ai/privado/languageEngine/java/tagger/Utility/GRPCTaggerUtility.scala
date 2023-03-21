package ai.privado.languageEngine.java.tagger.Utility

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.model.{CatLevelOne, Language, NodeType, RuleInfo}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}

object GRPCTaggerUtility {
  def getGrpcEndpoints(cpg: Cpg): List[Method] = {

    // `onCompleted` is always called inside of gRPC service methods
    // This `onCompleted` always has a StreamObserver in signature and it doesn't take any arguments
    val OnCompletedPattern = "(?i)(.*)(StreamObserver)(.*)(onCompleted)(.*)([(](0)[)])$"

    // Detecting `onCompleted` call by traversing function calls inside of all methods to narrow down gRPC services
    return cpg.method.where(_.call.methodFullName(OnCompletedPattern)).l
  }

  def getGrpcSinkCalls(cpg: Cpg, grpcEndpoints: List[Method]): ListBuffer[Call] = {
    // `onNext` is always called inside of gRPC service methods
    // This `onNext` always has a StreamObserver in signature and takes only one argument
    val OnNextPattern = "(?i)(.*)(StreamObserver)(.*)(onNext)(.*)([(](1)[)])$"
    var grpcSinkCalls = ListBuffer[Call]()

    grpcEndpoints.foreach(endpoint => {
      // Detecting gRPC API sink calls. These sink calls have the same name as function calls inside of onNext functions
      // To identify correct sinks, this filter makes sure sink call's parent is NOT `onNext`
      val callList = cpg.call(endpoint.name).whereNot(_.astParent.isCall.methodFullName(OnNextPattern)).l

      // Detecting `onNext` call inside of gRPC endpoint method
      // `onNext` takes on argument, this should be the call that satisfies gRPC/proto file contract
      // Get full type name of arguments that the server/endpoint will process
      val endpointArgTypes =
        endpoint.call.methodFullName(OnNextPattern).argument.isCall.argument.isIdentifier.typeFullName.l

      callList.foreach(sinkCall => {
        // Get full type name of arguments going inside gRPC sink call
        val sinkArgTypes = sinkCall.argument.isIdentifier.typeFullName.l
        if (endpointArgTypes.toSet == sinkArgTypes.toSet) {
          grpcSinkCalls += sinkCall
        }
      })

    })

    return grpcSinkCalls
  }

  def getGrpcSinkRules(cpg: Cpg): ListBuffer[RuleInfo] = {
    val endpoints = getGrpcEndpoints(cpg)
    var ruleList  = ListBuffer[RuleInfo]()

    getGrpcSinkCalls(cpg, endpoints).foreach(sink => {
      ruleList += RuleInfo(
        "Sinks.RPC.gRPC.Call",
        "gRPC Call",
        "",
        Array[String]("grpc.io"),
        List[String](sink.methodFullName),
        false,
        "",
        HashMap[String, String](),
        NodeType.REGULAR,
        "",
        CatLevelOne.SINKS,
        "api",
        Language.JAVA,
        Array[String]()
      )
    })

    return ruleList
  }
}
