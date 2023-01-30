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
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.BatchedUpdate.DiffGraphBuilder
import scala.collection.immutable.HashMap

class LoggerLombokPass(cpg: Cpg) extends CpgPass(cpg) {
  override def run(builder: DiffGraphBuilder): Unit = {
    val lombokLogger = cpg.annotation.name("CommonsLog|Flogger|Log|Log4j2|Slf4j").name.dedup.l
    if(lombokLogger.nonEmpty){
      val lombokLoggerType = lombokLogger.headOption.get
      val callNodes = cpg
        .identifier("log")
        .astParent
        .isCall
        .where(_.methodFullName("<unresolvedNamespace>.*"))
      callNodes.foreach(callNode => {
        updateNode(builder, callNode, lombokLoggerType)
      })
    }
  }

  def updateNode(builder: DiffGraphBuilder, node: Call, lombokLoggerType: String): Unit = {
    val loggerMap = HashMap("CommonsLog" -> "org.apache.commons.logging.Log",
      "Flogger"-> "com.google.common.flogger.FluentLogger",
      "Log"-> "java.util.logging.Logger",
      "Log4j2"-> "org.apache.logging.log4j.Logger",
      "Slf4j"->"org.slf4j.Logger")

    builder.setNodeProperty(
      node,
      PropertyNames.MethodFullName,
      node.methodFullName.replace("<unresolvedNamespace>", loggerMap.getOrElse(lombokLoggerType, "<unresolvedNamespace>"))
    )
  }
}
