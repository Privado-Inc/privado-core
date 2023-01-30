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

package ai.privado.languageEngine.java.passes.methodFullName

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.passes.{CpgPass, ForkJoinParallelCpgPass, SimpleCpgPass}
import io.shiftleft.semanticcpg.language._

class LoggerLombokPass(cpg: Cpg) extends CpgPass(cpg) {

  override def run(builder: DiffGraphBuilder): Unit = {
    val callNodes = cpg
      .identifier("log")
      .astParent
      .isCall
      .where(_.methodFullName("<unresolvedNamespace>.*"))
    callNodes.foreach(callNode => {
      updateNode(builder, callNode)
    })
  }

  def updateNode(builder: DiffGraphBuilder, node: Call): Unit = {
    builder.setNodeProperty(
      node,
      PropertyNames.MethodFullName,
      node.methodFullName.replace("<unresolvedNamespace>", "org.slf4j.Logger")
    )
  }
}
