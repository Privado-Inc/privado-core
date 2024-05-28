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

package ai.privado.tagger.utility

import ai.privado.cache.RuleCache
import ai.privado.dataflow.DuplicateFlowProcessor
import ai.privado.entrypoint.{PrivadoInput, ScanProcessor}
import ai.privado.languageEngine.java.language.NodeToProperty
import ai.privado.languageEngine.java.semantic.JavaSemanticGenerator
import ai.privado.model.{Constants, FilterProperty, RuleInfo, InternalTag}
import ai.privado.utility.Utilities.{
  addRuleTags,
  getDomainFromString,
  getFileNameForNode,
  isFileProcessable,
  storeForTag
}
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, CfgNode, JavaProperty, Member, Identifier}
import overflowdb.BatchedUpdate
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.codepropertygraph.generated.Cpg
import ai.privado.languageEngine.java.language._
import io.shiftleft.semanticcpg.language._

object SourceTaggerUtility {

  def getFilteredSourcesByTaggingDisabled(sources: List[AstNode]): List[AstNode] = {
    val hasExternalRule = sources.exists(_.tag.nameExact(Constants.externalRule).nonEmpty)

    if (hasExternalRule) {
      sources
    } else {
      sources.filterNot(_.tag.nameExact(InternalTag.TAGGING_DISABLED_BY_DED.toString).nonEmpty)
    }
  }

}
