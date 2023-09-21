package ai.privado.languageEngine.javascript.passes.read

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.dataflow.Dataflow
import ai.privado.entrypoint.ScanProcessor
import ai.privado.model.InternalTag
import ai.privado.model.sql.SQLQuery
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.{SQLParser, Utilities}
import ai.privado.utility.Utilities.{addRuleTags, storeForTag}
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.{EngineConfig, EngineContext}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.{Logger, LoggerFactory}

import sangria.parser.QueryParser
import sangria.ast._
import sangria.ast
import sangria.renderer.QueryRenderer
import scala.util.{Success, Failure}

class GraphqlQueryParserPass(cpg: Cpg, ruleCache: RuleCache, taggerCache: TaggerCache)
    extends PrivadoParallelCpgPass[Expression](cpg) {

  val sensitiveClassesWithMatchedRules = taggerCache.typeDeclMemberCache
  val sensitiveClasses                 = taggerCache.typeDeclMemberCache.keys
  val selectRegexPattern               = "(?i).*(mutation|query).*"

  val logger: Logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[_ <: AnyRef] = {
    // CPG query to fetch the Literal with GQL string
    // + Identify the arguments getting passed to GQL method calls.
    cpg.literal.code(selectRegexPattern).toArray ++
      cpg.call("(?i)(buildSchema|gql)").argument.isLiteral.toArray
  }

  override def runOnPart(builder: DiffGraphBuilder, node: Expression): Unit = {
    val gqlStr = node.code.stripMargin.replaceAll("\"", "").trim

    // Parse GraphQL query
    QueryParser.parse(gqlStr) match {
      case Success(document: Document) =>
        // Extract elements from the query. All Possible type of Fields
        // 1. Field
        // 2.Argument
        // 3. VariableDefinition
        // 4. OperationDefinition
        // 5. ObjectTypeDefinition
        // 6. InputValueDefinition
        // For tagging purpose we will consider fields & variables only.

        // Checking for OperationDefinition
        // query {
        //   bookings {
        //    createdAt
        //    event {
        //      title
        //      date
        //      price
        //    }
        //   }
        // }
        document.definitions.flatMap {
          case OperationDefinition(operationType, name, variables, _, selections, _, _, _) =>
            val operation = s"Operation: ${name.getOrElse("Unnamed")}, Operation Type: ${operationType.toString}}"
            // val args = extractArguments(variables.toList)
            val variableDefs = extractVariableDefinitions(variables.toList)
            val objectTypeDefs = extractObjectTypeDefinitions(document.definitions.collect {
              case objDef: ObjectTypeDefinition => objDef
            }.toList)
            val inputValueDefs = extractInputValueDefinitions(document.definitions.collect {
              case inputValueDef: InputValueDefinition => inputValueDef
            }.toList)
            val fields = extractFields(selections.toList)

//            println(operation)
//            variableDefs.foreach(i => println("Variables: " + i))
//            objectTypeDefs.foreach(i => println("Object Type: " + i))
//            inputValueDefs.foreach(i => println("InputValueDefs: " + i))
//            fields.foreach(i => println("Fields: " + i))

            // Process the GQL Nodes
            processGQLReadNode(builder, node, name.getOrElse("Unnamed"), fields ++ variableDefs)
            List(operation) ++ variableDefs ++ objectTypeDefs ++ inputValueDefs ++ fields
          case _ => Nil
        }

        // Checking for ObjectTypeDefinition
        // type User {
        //   _id: ID!
        //   email: String!
        //   password: String
        //   createdEvents: [Event!]
        // }
        document.definitions.collect { case objDef: ObjectTypeDefinition =>
          extractObjectTypeDetails(builder, node, objDef)
        }.flatten

      case Failure(error) =>
        logger.info(s"Syntax error: ${error.getMessage}")
    }
  }

  // Function to extract fields from ObjectTypeDefinition
  def extractObjectTypeDetails(
    builder: DiffGraphBuilder,
    node: Expression,
    objDef: ObjectTypeDefinition
  ): List[String] = {
    val typeName = objDef.name
    val fields = objDef.fields.flatMap {
      case field: FieldDefinition =>
        val fieldName      = field.name
        val fieldType      = field.fieldType
        val inputValueDefs = extractInputValueDefinitions(field.arguments.toList)
        s"Field: $typeName.$fieldName" :: inputValueDefs.map(s => s"$typeName.$fieldName.$s")
      case null => Nil
    }

    processGQLReadNode(builder, node, typeName, fields.toList)
    s"Object Type: $typeName" +: fields.toList
  }

  // Function to extract fields
  def extractFields(selections: List[Selection]): List[String] = {
    selections.flatMap {
      case field: Field =>
        val nestedFields = extractFields(field.selections.toList)
        // field.name :: nestedFields.map(nestedField => s"${field.name}.$nestedField")
        field.name :: nestedFields.map(nestedField => nestedField)
      case _ => Nil
    }
  }

  // Function to extract variable definitions
  def extractVariableDefinitions(variableDefinitions: List[VariableDefinition]): List[String] = {
    variableDefinitions.map(definition => definition.name)
  }

  // Function to extract object type definitions
  def extractObjectTypeDefinitions(definitions: List[ObjectTypeDefinition]): List[String] = {
    definitions.map(definition => definition.name)
  }

  // Function to extract input value definitions
  def extractInputValueDefinitions(definitions: List[InputValueDefinition]): List[String] = {
    definitions.map(definition => definition.name)
  }

  def processGQLReadNode(builder: DiffGraphBuilder, node: Expression, tableName: String, columns: List[String]) = {
    // Match classes which end with tableName/ObjectName
    val sensitiveMemberRuleIds = sensitiveClasses.find(s => s.matches(s"(?i).*${tableName}")) match {
      case Some(value) => sensitiveClassesWithMatchedRules(value).keys.l
      case None        => List.empty
    }

    if (columns.length == 1 && columns(0) == "*") {
      if (sensitiveMemberRuleIds.nonEmpty)
        sensitiveMemberRuleIds.foreach(ruleId => addTagsToNode(ruleId, node, builder))
      else {
        /* Run dataflow and verify the data-elements read from the call,
              Ex - resultSet = statement.executeQuery("SELECT * FROM mytable");
              // Loop through the result set and print out each row
              while (resultSet.next()) {
                  int id = resultSet.getInt("id");
                  String firstName = resultSet.getString("name");
                  int age = resultSet.getInt("age");
                  System.out.println("ID: " + id + ", Name: " + firstName + ", Age: " + age)
              }
         */
        val dataElementSinks =
          Dataflow
            .getSources(cpg)
            .filterNot(_.isMember)
            .map(_.asInstanceOf[CfgNode])
            .l

        val readFlow = dataElementSinks.reachableByFlows(node)(Utilities.getEngineContext(4)).l
        if (readFlow.nonEmpty) {
          // As a flow is present from Select query to a Data element we can say, the data element is read from the query
          readFlow
            .flatMap(_.elements.last.tag.value("Data.Sensitive.*"))
            .value
            .foreach(ruleId => addTagsToNode(ruleId, node, builder))
        }
      }
    } else {
      if (sensitiveMemberRuleIds.nonEmpty)
        sensitiveMemberRuleIds
          .filter(ruleId => isColumnNameMatchingWithRule(ruleId, columns))
          .foreach(ruleId => addTagsToNode(ruleId, node, builder))
      else
        ruleCache.getRule.sources
          .filter(rule => isColumnNameMatchingWithRule(rule.id, columns))
          .foreach(rule => addTagsToNode(rule.id, node, builder))

    }
  }

  /** Return True if any column name matches the pattern
    *
    * @param ruleId
    * @param columns
    * @return
    */
  def isColumnNameMatchingWithRule(ruleId: String, columns: List[String]): Boolean = {
    val pattern = ruleCache.getRuleInfo(ruleId).get.combinedRulePattern.r
    columns.map(pattern.matches).foldLeft(false)(_ || _)
  }

  def addTagsToNode(ruleId: String, node: Expression, builder: DiffGraphBuilder) = {
    storeForTag(builder, node, ruleCache)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
    addRuleTags(builder, node, ruleCache.getRuleInfo(ruleId).get, ruleCache)
  }

}
