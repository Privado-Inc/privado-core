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

package ai.privado.languageEngine.java.passes.config

import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.passes.PropertyEnvLinkerPassBase
import ai.privado.tagger.PrivadoParallelCpgPass
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import overflowdb.BatchedUpdate

/** This pass creates a graph layer for Java `.properties` files.
  */
class JavaEnvPropertyLinkerPass(cpg: Cpg) extends PropertyEnvLinkerPassBase(cpg) {

  override def generateParts(): Array[_ <: AnyRef] = {
    cpg.literal
      .filter(_.inCall.name(getMatchingLiteral).nonEmpty)
      .toArray
  }

  override def getMatchingLiteral: String = {
    "(?i).*(getProperty|getenv)"
  }

  override def connectProperties(node: AstNode, builder: DiffGraphBuilder): Unit = {
    val propertyKey = getPropertyKeyFromEnvCall(node)
    cpg.property
      .filter(p => p.name.nonEmpty && p.value.nonEmpty && propertyKey.matches(p.name))
      .foreach(p => {
        connectEnvProperty(node, p, builder)
      })
  }
}
