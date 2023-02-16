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

import io.shiftleft.codepropertygraph.schema._
import overflowdb.schema.EdgeType.Cardinality
import overflowdb.schema.Property.ValueType
import overflowdb.schema.SchemaBuilder

class CpgExtSchema(builder: SchemaBuilder, cpgSchema: CpgSchema) {
  import cpgSchema.ast._
  import cpgSchema.base._
  import cpgSchema.fs._
  import cpgSchema.method._
  import cpgSchema.typeSchema._

  // Add node types, edge types, and properties here

  val myProperty = builder
    .addProperty(name = "MYPROPERTY", valueType = ValueType.String)
    .mandatory("")

  val myNodeType = builder
    .addNodeType("MYNODETYPE")
    .addProperty(myProperty)

  val myPwdProperty = builder
    .addProperty(name = "PASSWORD", valueType = ValueType.String)
    .mandatory("")

  val myAwsKeysProperty = builder
    .addProperty(name = "AWS_KEYS", valueType = ValueType.String)
    .mandatory("")

  val myEdge = builder.addEdgeType("IS_CREDENTIAL").protoId(9999)

  val myCredNodeType = builder
    .addNodeType("CREDENTIALS")
    .addProperty(myPwdProperty)
    .addProperty(myAwsKeysProperty)
    .addProperty(code)

  literal.addOutEdge(edge = myEdge, inNode = myCredNodeType, cardinalityIn = Cardinality.ZeroOrOne)

  // Node and edge types for `.properties` files

  val property = builder
    .addNodeType("JAVA_PROPERTY")
    .addProperty(name)
    .addProperty(value)

  val isUsedAt = builder
    .addEdgeType("IS_USED_AT")

  val originalProperty = builder
    .addEdgeType("ORIGINAL_PROPERTY")

  property.addOutEdge(edge = sourceFile, inNode = file)
  property.addOutEdge(edge = isUsedAt, inNode = literal)
  property.addOutEdge(edge = isUsedAt, inNode = methodParameterIn)
  literal.addOutEdge(edge = originalProperty, inNode = property)
  methodParameterIn.addOutEdge(edge = originalProperty, inNode = property)
}

object CpgExtSchema {
  val builder   = new SchemaBuilder(domainShortName = "Cpg", basePackage = "io.shiftleft.codepropertygraph.generated")
  val cpgSchema = new CpgSchema(builder)
  val cpgExtSchema = new CpgExtSchema(builder, cpgSchema)
  val instance     = builder.build
}
