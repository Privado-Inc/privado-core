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

package ai.privado.cache

import io.shiftleft.codepropertygraph.generated.nodes.{Member, TypeDecl}
import scala.collection.concurrent.TrieMap
import scala.collection.mutable

import java.util.concurrent.ConcurrentHashMap
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala

class TaggerCache {
  // Stores typeDeclFullName --> ( sourceRuleId --->  Member Node)
  val typeDeclMemberCache = TrieMap[String, TrieMap[String, mutable.Set[Member]]]()

  // Stores typeDeclFullName --> (sourceRuleId ---> Extending TypeDecl Node)
  val typeDeclExtendingTypeDeclCache = TrieMap[String, TrieMap[String, TypeDecl]]()

  // Stores typeDeclFullName --> TypeDeclNode
  val typeDeclDerivedByExtendsCache = TrieMap[String, TypeDecl]()

  /** Checks and adds an item to Type decl member cache
    *
    * @param typeDeclVal
    * @param ruleId
    * @param typeDeclMember
    */
  def addItemToTypeDeclMemberCache(typeDeclVal: String, ruleId: String, typeDeclMember: Member): Unit = {
    println("typeDeclVal: " + typeDeclVal + " ruleId: " + ruleId + " typeDeclMember: " + typeDeclMember)
    typeDeclMemberCache.getOrElseUpdate(typeDeclVal, TrieMap[String, mutable.Set[Member]]()).updateWith(ruleId) {
      case Some(existingMembers) => Some(existingMembers += typeDeclMember)
      case None                  => Some(mutable.Set(typeDeclMember))
    }
  }

  def getTypeDeclMemberCacheItem(typeDeclVal: String): Map[String, Set[Member]] = {
    typeDeclMemberCache.getOrElse(typeDeclVal, TrieMap.empty).view.mapValues(_.toSet).toMap
  }

  def getTypeDeclExtendingTypeDeclCache: Map[String, Map[String, TypeDecl]] = {
    typeDeclExtendingTypeDeclCache.view.mapValues(_.toMap).toMap
  }

  def getTypeDeclExtendingTypeDeclCacheItem(typeFullName: String): TrieMap[String, TypeDecl] = {
    typeDeclExtendingTypeDeclCache.getOrElse(typeFullName, TrieMap[String, TypeDecl]())
  }

  def getTypeDeclDerivedByExtendsCache: Map[String, TypeDecl] = {
    typeDeclDerivedByExtendsCache.toMap
  }

}
