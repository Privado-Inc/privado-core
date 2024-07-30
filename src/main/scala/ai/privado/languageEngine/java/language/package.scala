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

package ai.privado.languageEngine.java

import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, NodeTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{AstNode, CfgNode, File, JavaProperty, Literal, MethodParameterIn}
import overflowdb.traversal._

import io.shiftleft.semanticcpg.language.*

import scala.jdk.CollectionConverters.IteratorHasAsScala

package object language {

  implicit class NodeStarters(cpg: Cpg) {
    def property: Traversal[JavaProperty] =
      cpg.graph.nodes(NodeTypes.JAVA_PROPERTY).asScala.cast[JavaProperty]
  }

  implicit class StepsForProperty(val trav: Traversal[JavaProperty]) extends AnyVal {

    def usedAt: Traversal[CfgNode] = trav.out(EdgeTypes.IS_USED_AT).cast[CfgNode]
    def file: Traversal[File]      = trav.out(EdgeTypes.SOURCE_FILE).cast[File]

  }

  implicit class NodeTravToProperty(val trav: Traversal[AstNode]) {
    def originalProperty: Traversal[JavaProperty] = trav.out(EdgeTypes.ORIGINAL_PROPERTY).cast[JavaProperty]
  }

  implicit class NodeToProperty(val node: AstNode) {
    def originalProperty: Option[JavaProperty] = {
      val property = node.out(EdgeTypes.ORIGINAL_PROPERTY)
      if (property != null && property.hasNext) {
        val prop = property.next()
        if (prop.isInstanceOf[JavaProperty]) {
          return Some(prop.asInstanceOf[JavaProperty])
        }
      }
      None
    }
    def originalPropertyValue: Option[String] = {
      val property = node.out(EdgeTypes.ORIGINAL_PROPERTY)
      if (property != null && property.hasNext) {
        val prop = property.next()
        if (prop.isInstanceOf[JavaProperty]) {
          return Some(prop.asInstanceOf[JavaProperty].value)
        }
      }
      None
    }
  }

  implicit class NodeToOriginalSource(val node: AstNode) extends AnyVal {
    def originalSource: Option[AstNode] = {
      val _originalSource = node.out(EdgeTypes.ORIGINAL_SOURCE)
      if (_originalSource.nonEmpty && _originalSource.hasNext) {
        return Option(_originalSource.next().asInstanceOf[AstNode])
      }
      None
    }

    def originalSource(sourceId: String): Option[AstNode] = {
      val _originalSource = node.out(EdgeTypes.ORIGINAL_SOURCE)
      if (_originalSource.nonEmpty && _originalSource.hasNext) {
        return _originalSource
          .find(node => node.asInstanceOf[AstNode].tag.nameExact("id").value(sourceId).nonEmpty)
          .asInstanceOf[Option[AstNode]]
      }
      None
    }
  }

  implicit class OriginalToDerivedSource(val node: AstNode) extends AnyVal {
    def derivedSource: Option[AstNode] = {
      val _derivedSource = node.out(EdgeTypes.DERIVED_SOURCE)
      if (_derivedSource.nonEmpty && _derivedSource.hasNext) {
        return Option(_derivedSource.next().asInstanceOf[AstNode])
      }
      None
    }
  }

  implicit class StepsForAstNode(val trav: Traversal[AstNode]) extends AnyVal {
    def originalSource: Traversal[AstNode] = trav.out(EdgeTypes.ORIGINAL_SOURCE).collectAll[AstNode]
    def derivedSource: Traversal[AstNode]  = trav.out(EdgeTypes.DERIVED_SOURCE).collectAll[AstNode]
  }
}
