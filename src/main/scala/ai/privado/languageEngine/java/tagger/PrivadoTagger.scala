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

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor, TimeMetric}
import ai.privado.languageEngine.java.feeder.StorageInheritRule
import ai.privado.languageEngine.java.passes.read.{DatabaseQueryReadPass, DatabaseRepositoryReadPass, EntityMapper}
import ai.privado.languageEngine.java.tagger.collection.{CollectionTagger, GrpcCollectionTagger, SOAPCollectionTagger}
import ai.privado.languageEngine.java.tagger.sink.{InheritMethodTagger, JavaAPITagger}
import ai.privado.languageEngine.java.tagger.source.{IdentifierTagger, InSensitiveCallTagger}
import ai.privado.tagger.PrivadoBaseTagger
import ai.privado.tagger.collection.WebFormsCollectionTagger
import ai.privado.tagger.config.DBConfigTagger
import ai.privado.tagger.sink.RegularSinkTagger
import ai.privado.tagger.source.{LiteralTagger, SqlQueryTagger}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Tag
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory
import overflowdb.traversal.Traversal

import java.util.Calendar

class PrivadoTagger(cpg: Cpg) extends PrivadoBaseTagger {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def runTagger(
    ruleCache: RuleCache,
    taggerCache: TaggerCache,
    privadoInputConfig: PrivadoInput
  ): Traversal[Tag] = {

    logger.info("Starting tagging")

    println(s"${TimeMetric.getNewTimeAndSetItToStageLast()} - --LiteralTagger invoked...")
    new LiteralTagger(cpg, ruleCache).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --LiteralTagger is done in \t\t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )
    println(s"${Calendar.getInstance().getTime} - --SqlQueryTagger invoked...")
    new SqlQueryTagger(cpg, ruleCache).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --SqlQueryTagger is done in \t\t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )
    println(s"${Calendar.getInstance().getTime} - --IdentifierTagger invoked...")
    new IdentifierTagger(cpg, ruleCache, taggerCache).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --IdentifierTagger is done in \t\t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )

    println(s"${Calendar.getInstance().getTime} - --InSensitive call tagger invoked...")
    new InSensitiveCallTagger(cpg, ruleCache, taggerCache).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --InSensitive call tagger is done in \t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )

    println(s"${Calendar.getInstance().getTime} - --DBConfigTagger invoked...")
    new DBConfigTagger(cpg).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --DBConfigTagger is done in \t\t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )

    println(s"${Calendar.getInstance().getTime} - --RegularSinkTagger invoked...")
    new RegularSinkTagger(cpg, ruleCache).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --RegularSinkTagger is done in \t\t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )

    println(s"${Calendar.getInstance().getTime} - --APITagger invoked...")
    new JavaAPITagger(cpg, ruleCache, privadoInputConfig).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --APITagger is done in \t\t\t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )
    // Custom Rule tagging
    if (!ScanProcessor.config.ignoreInternalRules) {
      // Adding custom rule to cache
      StorageInheritRule.rules.foreach(ruleCache.setRuleInfo)
      println(s"${Calendar.getInstance().getTime} - --CustomInheritTagger invoked...")
      new InheritMethodTagger(cpg, ruleCache).createAndApply()
      println(
        s"${TimeMetric.getNewTime()} - --CustomInheritTagger is done in \t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
      )
    }

    println(s"${Calendar.getInstance().getTime} - --Database Query Read Pass invoked...")
    new DatabaseQueryReadPass(cpg, ruleCache, taggerCache, EntityMapper.getClassTableMapping(cpg)).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --Database Query Read Pass is done in \t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )

    println(s"${Calendar.getInstance().getTime} - --Database Repository Read Pass invoked...")
    new DatabaseRepositoryReadPass(cpg, taggerCache).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --Database Repository Read Pass is done in \t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )

    println(s"${Calendar.getInstance().getTime} - --CollectionTagger invoked...")
    new CollectionTagger(cpg, ruleCache).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --CollectionTagger is done in \t\t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )

    println(s"${Calendar.getInstance().getTime} - --SOAPCollectionTagger invoked...")
    new SOAPCollectionTagger(cpg, ruleCache).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --SOAPCollectionTagger is done in \t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )

    println(s"${Calendar.getInstance().getTime} - --GrpcCollectionTagger invoked...")
    new GrpcCollectionTagger(cpg, ruleCache).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --GrpcCollectionTagger is done in \t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )
    println(s"${Calendar.getInstance().getTime} - --WebFormsCollectionTagger invoked...")
    new WebFormsCollectionTagger(cpg, ruleCache).createAndApply()
    println(
      s"${TimeMetric.getNewTime()} - --WebFormsCollectionTagger is done in \t\t- ${TimeMetric.setNewTimeToStageLastAndGetTimeDiff()}"
    )
    logger.info("Done with tagging")

    cpg.tag
  }

}
