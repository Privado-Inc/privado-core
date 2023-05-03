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

import Constants.CpgSchemaConstants
import io.shiftleft.codepropertygraph.schema._
import overflowdb.schema.EdgeType.Cardinality
import overflowdb.schema.Property.ValueType
import overflowdb.schema.SchemaBuilder

class CpgExtSchema(builder: SchemaBuilder, cpgSchema: CpgSchema) {

  // Use these imports to leverage the existing edges, nodes, classes etc
  import cpgSchema.ast._
  import cpgSchema.base._
  import cpgSchema.fs._
  import cpgSchema.method._
  import cpgSchema.tagsAndLocation._
  import cpgSchema.typeSchema._
  import cpgSchema.hidden._
  // Add node types, edge types, and properties here

  val myProperty = builder
    .addProperty(name = CpgSchemaConstants.MY_PROPERTY_NAME, valueType = ValueType.String)
    .mandatory(CpgSchemaConstants.MANDATORY_EMPTY_VALUE)

  val myNodeType = builder
    .addNodeType(CpgSchemaConstants.MY_NODE_TYPE_NAME)
    .addProperty(myProperty)

  val myPwdProperty = builder
    .addProperty(name = CpgSchemaConstants.MY_PASSWORD_NAME, valueType = ValueType.String)
    .mandatory(CpgSchemaConstants.MANDATORY_EMPTY_VALUE)

  val myAwsKeysProperty = builder
    .addProperty(name = CpgSchemaConstants.AWS_KEYS_NAME, valueType = ValueType.String)
    .mandatory(CpgSchemaConstants.MANDATORY_EMPTY_VALUE)

  val myEdge = builder.addEdgeType(CpgSchemaConstants.IS_CREDENTIAL_NAME).protoId(9999)

  val myCredNodeType = builder
    .addNodeType(CpgSchemaConstants.CREDENTIAL_NODE_NAME)
    .addProperty(myPwdProperty)
    .addProperty(myAwsKeysProperty)
    .addProperty(code)

  literal.addOutEdge(edge = myEdge, inNode = myCredNodeType, cardinalityIn = Cardinality.ZeroOrOne)

  // Node and edge types for `.properties` files

  // Adding Property Node start
  val property = builder
    .addNodeType(CpgSchemaConstants.JAVA_PROPERTY_NODE_NAME)
    .addProperty(name)
    .addProperty(value)
    .extendz(astNode)

  val isUsedAt = builder
    .addEdgeType(CpgSchemaConstants.IS_USED_AT_EDGE_NAME)

  val originalProperty = builder
    .addEdgeType(CpgSchemaConstants.ORIGINAL_PROPERTY_EDGE_NAME)

  property.addOutEdge(edge = sourceFile, inNode = file)
  property.addOutEdge(edge = isUsedAt, inNode = astNode)

  member.addOutEdge(edge = originalProperty, inNode = astNode)
  literal.addOutEdge(edge = originalProperty, inNode = astNode)
  methodParameterIn.addOutEdge(edge = originalProperty, inNode = astNode)
  callNode.addOutEdge(edge = originalProperty, inNode = astNode)

  // Adding Property Node End

  // Adding SQL query node Start

  val sqlColumnNode = builder
    .addNodeType(CpgSchemaConstants.SQL_COLUMN_NODE_NAME)
    .addProperty(name)
    .extendz(astNode)

  val sqlTableNode = builder
    .addNodeType(CpgSchemaConstants.SQL_TABLE_NODE_NAME)
    .addProperty(name)
    .extendz(astNode)

  val sqlQueryNode = builder
    .addNodeType(CpgSchemaConstants.SQL_QUERY_NODE_NAME)
    .addProperty(name)
    .extendz(astNode) // We are extending the new node from AstNode

  sqlColumnNode.addOutEdge(edge = sourceFile, inNode = file)
  sqlColumnNode.addOutEdge(edge = taggedBy, inNode = tag)

  sqlTableNode.addOutEdge(edge = sourceFile, inNode = file)
  sqlTableNode.addOutEdge(edge = taggedBy, inNode = tag)

  sqlQueryNode.addOutEdge(edge = sourceFile, inNode = file)
  sqlQueryNode.addOutEdge(edge = taggedBy, inNode = tag)

  sqlQueryNode.addOutEdge(edge = ast, inNode = sqlTableNode)
  sqlTableNode.addOutEdge(edge = ast, inNode = sqlColumnNode)

  // Adding SQL query node End

  val groupId = builder
    .addProperty(name = CpgSchemaConstants.MODULE_GROUP_ID_NAME, valueType = ValueType.String)
    .mandatory(CpgSchemaConstants.MANDATORY_EMPTY_VALUE)

  val artifactId = builder
    .addProperty(name = CpgSchemaConstants.MODULE_ARTIFACT_ID_NAME, valueType = ValueType.String)
    .mandatory(CpgSchemaConstants.MANDATORY_EMPTY_VALUE)

  val configVersion = builder
    .addProperty(name = CpgSchemaConstants.MODULE_CONFIG_VERSION_NAME, valueType = ValueType.String)

  val module = builder
    .addNodeType(CpgSchemaConstants.MODULE_NODE_NAME)
    .addProperty(groupId)
    .addProperty(artifactId)
    .addProperty(configVersion)

  val dependency = builder
    .addNodeType(CpgSchemaConstants.MODULE_DEPENDENCY_NODE_NAME)
    .addProperty(groupId)
    .addProperty(artifactId)
    .addProperty(configVersion)

  val dependencies = builder
    .addEdgeType(CpgSchemaConstants.MODULE_DEPENDENCY_EDGE_NAME)

  module.addOutEdge(edge = dependencies, inNode = dependency)
  module.addOutEdge(edge = sourceFile, inNode = file)
  dependency.addOutEdge(edge = sourceFile, inNode = file)
  templateDOM.addOutEdge(edge = sourceFile, inNode = file)

}

object CpgExtSchema {
  val builder = new SchemaBuilder(
    domainShortName = CpgSchemaConstants.CPG_DOMAIN_SHORT_NAME,
    basePackage = CpgSchemaConstants.CPG_BASE_PACKAGE_NAME
  )
  val cpgSchema    = new CpgSchema(builder)
  val cpgExtSchema = new CpgExtSchema(builder, cpgSchema)
  val instance     = builder.build
}
