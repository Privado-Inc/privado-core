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

package ai.privado.java.feeder

import ai.privado.model.{CatLevelOne, Language, NodeType, RuleInfo}

import scala.collection.immutable.HashMap

object StorageInheritRule {

  val rules = List(
    RuleInfo(
      "Sinks.Database.JPA.Read",
      "JPA Repository(Read)",
      "",
      Array[String]("springframework.org"),
      List[String](
        "org[.]springframework[.]data[.]repository[.]CrudRepository|org[.]springframework[.]data[.]jpa[.]repository[.]support[.]SimpleJpaRepository|org[.]springframework[.]data[.]jpa[.]repository[.]JpaRepository",
        "(find|get).*"
      ),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "storages",
      Language.JAVA,
      Array[String]()
    ),
    RuleInfo(
      "Sinks.Database.JPA.Write",
      "JPA Repository(Write)",
      "",
      Array[String]("springframework.org"),
      List[String](
        "org[.]springframework[.]data[.]repository[.]CrudRepository|org[.]springframework[.]data[.]jpa[.]repository[.]support[.]SimpleJpaRepository|org[.]springframework[.]data[.]jpa[.]repository[.]JpaRepository",
        "(save|delete).*"
      ),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "storages",
      Language.JAVA,
      Array[String]()
    ),
    RuleInfo(
      "Storages.MongoDB.SpringFramework.Read",
      "MongoDB(Read)",
      "",
      Array[String]("mongodb.com"),
      List[String](
        "(?i)(org[.]springframework[.]data[.]mongodb[.]repository[.](MongoRepository|support[.]SimpleMongoRepository))",
        "(?i)(find|count|exists).*"
      ),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "storages",
      Language.JAVA,
      Array[String]()
    ),
    RuleInfo(
      "Storages.MongoDB.SpringFramework.Write",
      "MongoDB(Write)",
      "",
      Array[String]("mongodb.com"),
      List[String](
        "(?i)(org[.]springframework[.]data[.]mongodb[.]repository[.](MongoRepository|support[.]SimpleMongoRepository))",
        "(?i)(insert|save|delete).*"
      ),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "storages",
      Language.JAVA,
      Array[String]()
    ),
    RuleInfo(
      "Storages.MongoDB.Morphia.BasicDao.Read",
      "MongoDB(Read)",
      "",
      Array[String]("mongodb.com"),
      List[String]("(?i)(.*[.]morphia[.]dao[.]BasicDAO.*)", "(?i)(count|createQuery|exists|find|get).*"),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "storages",
      Language.JAVA,
      Array[String]()
    ),
    RuleInfo(
      "Storages.MongoDB.Morphia.BasicDao.Write",
      "MongoDB(Write)",
      "",
      Array[String]("mongodb.com"),
      List[String]("(?i)(.*[.]morphia[.]dao[.]BasicDAO.*)", "(?i)(delete|save|update).*"),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "storages",
      Language.JAVA,
      Array[String]()
    ),
    RuleInfo(
      "Storages.Android.SQLite.SQLiteOpenHelper.Read",
      "SQLite(Read)",
      "",
      Array[String]("android.com"),
      List[String]("(?i)(android[.]database[.]sqlite[.]SQLiteOpenHelper)", "(query|rawQuery).*"),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "storages",
      Language.JAVA,
      Array[String]()
    ),
    RuleInfo(
      "Storages.Android.SQLite.SQLiteOpenHelper.Write",
      "SQLite(Write)",
      "",
      Array[String]("android.com"),
      List[String](
        "(?i)(android[.]database[.]sqlite[.]SQLiteOpenHelper)",
        "(delete|update|replace|insert|execSQL|execPerConnectionSQL|compileStatement).*"
      ),
      false,
      "",
      HashMap[String, String](),
      NodeType.REGULAR,
      "",
      CatLevelOne.SINKS,
      "storages",
      Language.JAVA,
      Array[String]()
    )
  )
}
