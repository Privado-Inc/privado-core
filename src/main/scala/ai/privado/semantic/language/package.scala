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
 */

package ai.privado.semantic

import ai.privado.dataflow.Dataflow
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, EdgeTypes, NodeTypes}
import io.shiftleft.semanticcpg.language.{DefaultNodeExtensionFinder, NodeExtensionFinder}
import overflowdb.traversal.*

import scala.jdk.CollectionConverters.IteratorHasAsScala
import scala.util.Try

package object language {

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder

  implicit class NodeStarterForSqlQueryNode(cpg: Cpg) {
    def sqlQuery: Traversal[SqlQueryNode] =
      Try(cpg.graph.nodes(NodeTypes.SQL_QUERY_NODE).asScala.cast[SqlQueryNode]).toOption
        .getOrElse(Iterator.empty[SqlQueryNode])

    def sqlTable: Traversal[SqlTableNode] =
      Try(cpg.graph.nodes(NodeTypes.SQL_TABLE_NODE).asScala.cast[SqlTableNode]).toOption
        .getOrElse(Iterator.empty[SqlTableNode])

    def sqlColumn: Traversal[SqlColumnNode] =
      Try(cpg.graph.nodes(NodeTypes.SQL_COLUMN_NODE).asScala.cast[SqlColumnNode]).toOption
        .getOrElse(Iterator.empty[SqlColumnNode])

  }

  implicit class NodeStarterForDBNode(cpg: Cpg) {
    def dbNode: Traversal[DbNode] =
      Try(cpg.graph.nodes(NodeTypes.DB_NODE).asScala.cast[DbNode]).getOrElse(Iterator.empty[DbNode])
  }

  implicit class StepsForPropertyForDbNode(val trav: Traversal[DbNode]) extends AnyVal {
    def file: Traversal[File] = Try(trav.out(EdgeTypes.SOURCE_FILE).cast[File]).toOption.getOrElse(Iterator.empty[File])

  }
  implicit class StepsForPropertyForSqlQueryNode(val trav: Traversal[SqlQueryNode]) extends AnyVal {
    def file: Traversal[File] = Try(trav.out(EdgeTypes.SOURCE_FILE).cast[File]).toOption.getOrElse(Iterator.empty[File])
    def sqlTable: Traversal[SqlTableNode] =
      Try(trav.out(EdgeTypes.AST).cast[SqlTableNode]).toOption.getOrElse(Iterator.empty[SqlTableNode])
  }

  implicit class StepsForPropertyForSqlTableNode(val trav: Traversal[SqlTableNode]) extends AnyVal {
    def file: Traversal[File] = Try(trav.out(EdgeTypes.SOURCE_FILE).cast[File]).toOption.getOrElse(Iterator.empty[File])
    def sqlColumn: Traversal[SqlColumnNode] =
      Try(trav.out(EdgeTypes.AST).cast[SqlColumnNode]).toOption.getOrElse(Iterator.empty[SqlColumnNode])
  }

  implicit class StepsForPropertyForSqlColumnNode(val trav: Traversal[SqlColumnNode]) extends AnyVal {
    def file: Traversal[File] = Try(trav.out(EdgeTypes.SOURCE_FILE).cast[File]).toOption.getOrElse(Iterator.empty[File])

  }

  // when there is no node of type AST then collectFirst method will return None

  implicit class SqlColumnObject(val node: SqlColumnNode) {
    def sqlTable: Option[SqlTableNode] =
      node.start.in(EdgeTypes.AST).collectFirst { case sqlNode: SqlTableNode => sqlNode }

  }

  implicit class SqlTableObject(val node: SqlTableNode) {
    def sqlQuery: Option[SqlQueryNode] =
      node.start.in(EdgeTypes.AST).collectFirst { case sqlNode: SqlQueryNode => sqlNode }

  }

  implicit class NodeStarterForAndroidXmlLayoutNode(cpg: Cpg) {
    def androidXmlLayoutNode: Traversal[AndroidXmlLayoutNode] =
      Try(cpg.graph.nodes(NodeTypes.ANDROID_XML_LAYOUT_NODE).asScala.cast[AndroidXmlLayoutNode]).toOption
        .getOrElse(Iterator.empty[AndroidXmlLayoutNode])
  }

  implicit class NodeStarterForAndroidXmlPermissionNode(cpg: Cpg) {
    def androidXmlPermissionNode: Traversal[AndroidXmlPermissionNode] =
      Try(cpg.graph.nodes(NodeTypes.ANDROID_XML_PERMISSION_NODE).asScala.cast[AndroidXmlPermissionNode]).toOption
        .getOrElse(Iterator.empty[AndroidXmlPermissionNode])
  }

}
