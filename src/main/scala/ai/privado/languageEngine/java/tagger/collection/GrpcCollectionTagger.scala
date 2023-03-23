/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 *
 */

package ai.privado.languageEngine.java.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{CatLevelOne, Constants, InternalTag, Language, NodeType, RuleInfo}
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import scala.collection.immutable.HashMap

class GrpcCollectionTagger(cpg: Cpg, sourceRuleInfos: List[RuleInfo]) extends CpgPass(cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def run(builder: DiffGraphBuilder): Unit = {

    /* We try to see if onCompleted call is there since this is called by methods that are
    handling the grpc calls when grpc-java is used. We further check if its called from a
    StreamObserver object. For eg. in the following case,

      public void sayHello(HelloRequest req, StreamObserver<HelloReply> responseObserver) {
        ...
        foo.onNext()
        foo.onCompleted();
      }

    we aim to find if foo's type is StreamObserver and then return the sayHello method as the
    GRPC collection point since it handles the request
     */

    val grpcCollectionMethods = cpg.call
      .name("onCompleted")
      .filter(_.argument.size == 1)
      .where(_.argument.typ.fullName(".*StreamObserver.*"))
      .method
      .l

    val methodFullNamesCombined = grpcCollectionMethods.map(sink => sink.fullName).mkString("(", "|", ")")

    // Create a hardcoded rule specially for GRPC so we adhere to CollectionExporter style of operation
    val ruleInfo = RuleInfo(
      "Collections.RPC.gRPC.Handler",
      "gRPC Handler",
      "",
      Array[String]("grpc.io"),
      List[String](methodFullNamesCombined),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      "GRPC",
      Language.JAVA,
      Array[String]()
    )

    // Tag parameters same way we do while tagging collections
    val grpcCollectionPoints = Traversal(grpcCollectionMethods).flatMap(collectionMethod => {
      sourceRuleInfos.flatMap(sourceRule => {
        val parameters =
          collectionMethod.parameter.where(_.name(sourceRule.combinedRulePattern)).whereNot(_.code("this")).l
        if (parameters.isEmpty) {
          None
        } else {
          parameters.foreach(parameter => storeForTag(builder, parameter)(Constants.id, sourceRule.id))
          Some(collectionMethod)
        }
      })
    })

    grpcCollectionPoints.foreach(collectionPoint => {
      addRuleTags(builder, collectionPoint, ruleInfo)
      storeForTag(builder, collectionPoint)(InternalTag.COLLECTION_METHOD_ENDPOINT.toString, collectionPoint.name)
    })
  }
}
