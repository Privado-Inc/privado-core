package ai.privado.feeder

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
      List[String]("(?i)(android[.]database[.]sqlite[.]SQLiteOpenHelper", "(query|rawQuery).*"),
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
        "(?i)(android[.]database[.]sqlite[.]SQLiteOpenHelper",
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
