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

package ai.privado.languageEngine.java.tagger

import ai.privado.cache.{DataFlowCache, RuleCache, S3DatabaseDetailsCache, TaggerCache}
import ai.privado.entrypoint.PrivadoInput
import ai.privado.feeder.PermissionSourceRule
import ai.privado.languageEngine.java.feeder.StorageInheritRule
import ai.privado.languageEngine.java.passes.read.{
  DatabaseQueryReadPass,
  DatabaseRepositoryReadPass,
  EntityMapper,
  MessagingConsumerReadPass
}
import ai.privado.languageEngine.java.tagger.collection.{
  CollectionTagger,
  GrpcCollectionTagger,
  MethodFullNameCollectionTagger,
  SOAPCollectionTagger
}
import ai.privado.languageEngine.java.tagger.config.JavaDBConfigTagger
import ai.privado.languageEngine.java.tagger.sink.{InheritMethodTagger, JavaAPITagger, MessagingConsumerCustomTagger}
import ai.privado.languageEngine.java.tagger.source.{IdentifierTagger, InSensitiveCallTagger}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.collection.{AndroidCollectionTagger, WebFormsCollectionTagger}
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.tagger.source.{AndroidXmlPermissionTagger, LiteralTagger, SqlQueryTagger}
import ai.privado.utility.Utilities.ingressUrls
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import java.nio.file.Paths

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    ruleCache: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput,
    dataFlowCache: DataFlowCache,
    s3DatabaseDetailsCache: S3DatabaseDetailsCache
  ): Traversal[Tag] = {

    logger.info("Starting tagging")

    new LiteralTagger(cpg, ruleCache).createAndApply()

    new SqlQueryTagger(cpg, ruleCache).createAndApply()

    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()

    new InSensitiveCallTagger(cpg, ruleCache, taggerCache).createAndApply()

    new JavaDBConfigTagger(cpg).createAndApply()

    new RegularSinkTagger(cpg, ruleCache).createAndApply()

    new JavaS3Tagger(cpg, s3DatabaseDetailsCache).createAndApply()

    new JavaAPITagger(cpg, ruleCache, privadoInputConfig).createAndApply()
    // Custom Rule tagging
    if (!privadoInputConfig.ignoreInternalRules) {
      // Adding custom rule to cache
      StorageInheritRule.rules.foreach(ruleCache.setRuleInfo)
      new InheritMethodTagger(cpg, ruleCache).createAndApply()
      new MessagingConsumerCustomTagger(cpg, ruleCache).createAndApply()
      new MessagingConsumerReadPass(cpg, taggerCache, dataFlowCache, privadoInputConfig).createAndApply()
    }

    new DatabaseQueryReadPass(cpg, ruleCache, taggerCache, privadoInputConfig, EntityMapper.getClassTableMapping(cpg))
      .createAndApply()

    new DatabaseRepositoryReadPass(cpg, taggerCache, dataFlowCache).createAndApply()

    val collectionTagger = new CollectionTagger(cpg, ruleCache)
    collectionTagger.createAndApply()
    ingressUrls = collectionTagger.getIngressUrls()

    val methodFullNameTagger = new MethodFullNameCollectionTagger(cpg, ruleCache)
    methodFullNameTagger.createAndApply()
    ingressUrls.appendedAll(methodFullNameTagger.getIngressUrls())

    new SOAPCollectionTagger(cpg, ruleCache).createAndApply()

    new GrpcCollectionTagger(cpg, ruleCache).createAndApply()

    new WebFormsCollectionTagger(cpg, ruleCache).createAndApply()

    new AndroidXmlPermissionTagger(cpg, ruleCache, PermissionSourceRule.miniatureRuleList).createAndApply()

    new AndroidCollectionTagger(
      cpg,
      Paths.get(privadoInputConfig.sourceLocation.head).toAbsolutePath.toString,
      ruleCache
    ).createAndApply()

    logger.info("Done with tagging")

    cpg.tag
  }

}
