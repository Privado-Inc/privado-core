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
import ai.privado.model.{CatLevelOne, Language, NodeType, RuleInfo}
import io.shiftleft.codepropertygraph.generated.Cpg
import org.slf4j.LoggerFactory
import ai.privado.languageEngine.java.tagger.Utility.GRPCTaggerUtility
import ai.privado.tagger.PrivadoSimpleCpgPass

import scala.collection.immutable.HashMap

class GrpcCollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoSimpleCpgPass(cpg) {
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

    val grpcCollectionMethods = GRPCTaggerUtility.getGrpcEndpoints(cpg)

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

    CollectionUtility.tagDirectSources(
      builder,
      grpcCollectionMethods,
      ruleCache.getRule.sources,
      ruleInfo,
      ruleCache,
      returnByName = true
    )
    CollectionUtility.tagDerivedSources(cpg, builder, grpcCollectionMethods, ruleInfo, ruleCache, returnByName = true)
  }
}
