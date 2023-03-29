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
      .where(_.parameter.typeFullName(StreamObserverPattern))
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
    val stubPattern   = "(?i)(.*)(stub)(.*)"
    val anyType       = "ANY"
    var grpcSinkCalls = ListBuffer[Call]()

    println(grpcEndpoints.length)

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

      println(sinks.length)

      // Detecting `onNext` call inside of gRPC endpoint method
      // `onNext` takes on argument, this should be the call that satisfies gRPC/proto file contract
      // Get full type name of arguments that the server/endpoint will process
      val endpointParamTypes = endpoint.parameter.typeFullName.l

      println(endpointParamTypes.length)

      sinks.foreach(sink => {
        // Get full type name of arguments going inside gRPC sink call
        val sinkArgTypes = sink.argument.isIdentifier.typeFullName.l
        sinkArgTypes.foreach(sinkArgType => {
          // Add sinks for matching sink args, unresolved args, or if they have a stub in args
          if (endpointParamTypes.contains(sinkArgType)) {
            grpcSinkCalls += sink
          } else if (sinkArgType.equals(anyType)) {
            grpcSinkCalls += sink
          } else if (sinkArgType.matches(stubPattern)) {
            grpcSinkCalls += sink
          }
        })

        // Add sinks if endpointParams are empty or if they don't get resolved
        if (endpointParamTypes.size == 0) {
          grpcSinkCalls += sink
        } else if (endpointParamTypes.equals(anyType)) {
          grpcSinkCalls += sink
        }
      })
    })

    return grpcSinkCalls.dedup.l
  }

  def getGrpcSinks(cpg: Cpg): List[Call] = {
    val endpoints = getGrpcEndpoints(cpg)
    return getGrpcSinkCalls(cpg, endpoints)
  }
}
