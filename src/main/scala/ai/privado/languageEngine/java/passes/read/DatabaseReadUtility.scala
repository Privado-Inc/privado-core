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

package ai.privado.languageEngine.java.passes.read

import ai.privado.cache.{RuleCache, TaggerCache}
import ai.privado.dataflow.Dataflow
import ai.privado.model.InternalTag
import ai.privado.model.sql.SQLQuery
import ai.privado.utility.SQLParser
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.nodes.{AnnotationLiteral, AstNode, CfgNode, TypeDecl}
import overflowdb.BatchedUpdate.DiffGraphBuilder
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.Cpg

object DatabaseReadUtility {

  val selectRegexPattern = "(?i)(\")?\\s{0,5}select\\s+.*"
  val fromRegexPattern   = "(?i)(\")?\\s{0,5}from\\s+[a-zA-Z.]{3,}\\s+.*"

  def processDBReadNode(
    builder: DiffGraphBuilder,
    ruleCache: RuleCache,
    taggerCache: TaggerCache,
    classTableMapping: Map[String, TypeDecl],
    cpg: Cpg,
    node: AstNode
  ) = {

    val queryCode = node.code
    val referencingQueryNodes = {
      if (node.isInstanceOf[AnnotationLiteral]) {
        /*
        Get the name of the namedQuery and use it to fetch the callNode which is used to make the namedQueryCall
        Ex - @Entity
        @Table(name = "student")

        //Using @NamedQuery for single JPQL or HQL
        @NamedQuery(name = "GET_STUDENTS_COUNT", query = "select count(1) from Student")
        class Student {}

        List<Long> totalStudents = session.createNamedQuery("GET_STUDENTS_COUNT", Long.class).getResultList();
         */
        val nameOfNamedQuery = node.astParent.astSiblings
          .where(_.astChildren.order(1).code("name"))
          .astChildren
          .collectAll[AnnotationLiteral]
          .code
          .headOption
          .getOrElse("")
        if (nameOfNamedQuery.nonEmpty) {
          cpg.method
            .fullName("org[.]hibernate[.].*create[a-zA-Z]{0,10}Query:.*")
            .callIn
            .where(_.argument.code("(\"){0,1}" + nameOfNamedQuery + "(\"){0,1}"))
            .l
        } else
          List()
      } else List(node)
    }

    val sensitiveClassesWithMatchedRules = taggerCache.typeDeclMemberCache
    val sensitiveClasses                 = taggerCache.typeDeclMemberCache.keys.l
    val query                            = extractSQLForConcatenatedString(queryCode)
    val result                           = SQLParser.parseSqlQuery(query)

    result match {
      case Some(value) =>
        value.foreach { case queryModel: SQLQuery =>
          // Match classes which end with tableNameRegex
          val tableName = queryModel.table.name
          val columns   = queryModel.column.map(_.name)

          val sensitiveMemberRuleIds = {
            if (
              classTableMapping.contains(tableName) && sensitiveClasses.contains(classTableMapping(tableName).fullName)
            )
              sensitiveClassesWithMatchedRules(classTableMapping(tableName).fullName).keys.l
            else
              sensitiveClasses.find(s => s.matches(s"(?i).*${tableName}")) match {
                case Some(value) => sensitiveClassesWithMatchedRules(value).keys.l
                case None        => List.empty
              }
          }

          if (columns.length == 1 && columns(0) == "*") {
            if (sensitiveMemberRuleIds.nonEmpty)
              sensitiveMemberRuleIds.foreach(ruleId => addTagsToNode(ruleCache, ruleId, referencingQueryNodes, builder))
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
                  .filter(_.isInstanceOf[CfgNode])
                  .map(_.asInstanceOf[CfgNode])
                  .l

              val readFlow = Dataflow.dataflowForSourceSinkPair(referencingQueryNodes, dataElementSinks)
              if (readFlow.nonEmpty) {
                // As a flow is present from Select query to a Data element we can say, the data element is read from the query
                readFlow
                  .flatMap(_.elements.last.tag.value("Data.Sensitive.*"))
                  .value
                  .foreach(ruleId => addTagsToNode(ruleCache, ruleId, referencingQueryNodes, builder))
              }
            }
          } else {
            if (sensitiveMemberRuleIds.nonEmpty)
              sensitiveMemberRuleIds
                .filter(ruleId => isColumnNameMatchingWithRule(ruleCache, ruleId, columns))
                .foreach(ruleId => addTagsToNode(ruleCache, ruleId, referencingQueryNodes, builder))
            else
              ruleCache.getRule.sources
                .filter(rule => isColumnNameMatchingWithRule(ruleCache, rule.id, columns))
                .foreach(rule => addTagsToNode(ruleCache, rule.id, referencingQueryNodes, builder))
          }
        }
      case None => ()
    }

  }

  /** Return True if any column name matches the pattern
    *
    * @param ruleId
    * @param columns
    * @return
    */
  def isColumnNameMatchingWithRule(ruleCache: RuleCache, ruleId: String, columns: List[String]): Boolean = {
    val pattern = ruleCache.getRuleInfo(ruleId).get.combinedRulePattern.r
    columns.map(pattern.matches).foldLeft(false)(_ || _)
  }

  def addTagsToNode(ruleCache: RuleCache, ruleId: String, nodes: List[AstNode], builder: DiffGraphBuilder) = {
    nodes.foreach(node => {
      storeForTag(builder, node, ruleCache)(InternalTag.VARIABLE_REGEX_LITERAL.toString)
      addRuleTags(builder, node, ruleCache.getRuleInfo(ruleId).get, ruleCache)
    })
  }

  def extractSQLForConcatenatedString(sqlQuery: String): String = {
    var query = sqlQuery
      .stripPrefix("\"")
      .stripSuffix("\"")
      .split("\\\"\\s*\\+\\s*\\\"") // Splitting the query on '+' operator and joining back to form complete query
      .map(_.stripMargin)
      .mkString("")

    // Add `select *` to queries which are `from users`
    if (query.matches(fromRegexPattern))
      query = "select * " + query

    val pattern =
      "(?i)SELECT\\s(.*?)\\sFROM\\s(.*?)(`.*?`|\".*?\"|'.*?'|\\w+)".r // Pattern to fetch the SELECT statement from the query
    pattern.findFirstIn(query).getOrElse("")
  }

}
