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

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{AnnotationLiteral, TypeDecl}
import io.shiftleft.semanticcpg.language._

object EntityMapper {

  /** Create a mapping of class Node and the Table name used in database
    *   1. It is retrieving all type declarations that are annotated with the "Entity" annotation.
    *
    * 2. Then it is mapping each type declaration to a tuple containing the type declaration and the table name
    * associated with it.
    *
    * 3. If the type declaration has an annotation with the "Table" name, the table name will be extracted from it and
    * used as the second element of the tuple. Otherwise, the type declaration's name will be used as the second element
    * instead.
    *
    * 4. Finally, the resulting list of tuples is returned.
    *
    * @param cpg
    * @returns
    *   A mapping of tableName => TypeDeclNode
    */
  def getClassTableMapping(cpg: Cpg): Map[String, TypeDecl] = {

    //  @Entity
    //  @Table(name = "student")
    //  class Student { }
    cpg.annotation
      .name("Entity")
      .typeDecl
      .map(typeDeclNode => {
        val tableName =
          typeDeclNode.annotation.name("Table").ast.collectAll[AnnotationLiteral].name.headOption.getOrElse("")
        // If tableName is not specified the class name is the table name
        if (tableName.isEmpty)
          (typeDeclNode.name.toLowerCase, typeDeclNode)
        else
          (tableName.toLowerCase, typeDeclNode)
      })
      .toMap

  }

}
