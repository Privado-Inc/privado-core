import io.shiftleft.codepropertygraph.schema._
import overflowdb.schema.EdgeType.Cardinality
import overflowdb.schema.Property.ValueType
import overflowdb.schema.SchemaBuilder

class CpgExtSchema(builder: SchemaBuilder, cpgSchema: CpgSchema) {
  import cpgSchema.ast._
  import cpgSchema.base._

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

}

object CpgExtSchema {
  val builder   = new SchemaBuilder(domainShortName = "Cpg", basePackage = "io.shiftleft.codepropertygraph.generated")
  val cpgSchema = new CpgSchema(builder)
  val cpgExtSchema = new CpgExtSchema(builder, cpgSchema)
  val instance     = builder.build
}
