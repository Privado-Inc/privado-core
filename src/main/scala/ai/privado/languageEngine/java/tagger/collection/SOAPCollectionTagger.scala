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

import ai.privado.languageEngine.java.tagger.Utility.SOAPTaggerUtility
import ai.privado.model.{CatLevelOne, Language, NodeType, RuleInfo}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPass
import org.slf4j.LoggerFactory
import io.shiftleft.semanticcpg.language._

import scala.collection.immutable.HashMap
import scala.collection.mutable

class SOAPCollectionTagger(cpg: Cpg, sourceRuleInfos: List[RuleInfo]) extends CpgPass(cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def run(builder: DiffGraphBuilder): Unit = {
    logger.info("Tagging collection points for SOAP")

    val soapCollectionMethods = SOAPTaggerUtility.getAPIMethods(cpg)

    // Create a hardcoded rule specially for SOAP so we adhere to CollectionExporter style of operation
    val ruleInfo = RuleInfo(
      "Collections.Annotation.SOAP",
      "SOAP Annotation",
      "",
      Array[String](),
      List[String]("WebService"),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.COLLECTIONS,
      "SOAP",
      Language.JAVA,
      Array[String]()
    )

    val classUrlMap  = mutable.HashMap[Long, String]()
    val methodUrlMap = mutable.HashMap[Long, String]()

    cpg.annotation
      .name(ruleInfo.combinedRulePattern)
      .filter(_.typeDecl.nonEmpty)
      .foreach(classAnnotation => {
        classUrlMap
          .addOne(classAnnotation.typeDecl.head.id() -> ("/" + CollectionUtility.getUrlFromAnnotation(classAnnotation)))
      })

    soapCollectionMethods.annotation
      .name("WebMethod")
      .foreach(methodAnnotation => {
        methodUrlMap
          .addOne(methodAnnotation.method.head.id() -> ("/" + CollectionUtility.getUrlFromAnnotation(methodAnnotation)))
      })
    CollectionUtility.tagDirectSources(
      builder,
      soapCollectionMethods,
      sourceRuleInfos,
      ruleInfo,
      classUrlMap = classUrlMap,
      methodUrlMap = methodUrlMap
    )
    CollectionUtility.tagDerivedSources(
      cpg,
      builder,
      soapCollectionMethods,
      ruleInfo,
      classUrlMap = classUrlMap,
      methodUrlMap = methodUrlMap
    )
  }
}
