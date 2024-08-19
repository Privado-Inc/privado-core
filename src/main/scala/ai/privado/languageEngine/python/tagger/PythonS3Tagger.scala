package ai.privado.languageEngine.python.tagger

import ai.privado.tagger.S3BaseTagger
import ai.privado.cache.{DatabaseDetailsCache, S3DatabaseDetailsCache}
import ai.privado.semantic.language.*
import ai.privado.model.DatabaseDetails
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages, Operators}
import org.slf4j.LoggerFactory
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import overflowdb.traversal.*
import scala.collection.mutable.ListBuffer

class PythonS3Tagger(
  cpg: Cpg,
  s3DatabaseDetailsCache: S3DatabaseDetailsCache,
  databaseDetailsCache: DatabaseDetailsCache
) extends S3BaseTagger(cpg, s3DatabaseDetailsCache, databaseDetailsCache) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def run(builder: DiffGraphBuilder): Unit = {
    val readWriteBucketNames = ListBuffer[String]()

    val bucketArgs = cpg.call
      .where(_.tag.valueExact("Storages.AmazonS3.ReadAndWrite"))
      .argument
      .isIdentifier
      .name(".*bucket.*")
      .l
    // handle case where Bucket() call's arg is assigned bucket name, eg.
    //    Bucket(MY_BUCKET_NAME)
      ++ cpg.call
        .where(_.tag.valueExact("Storages.AmazonS3.ReadAndWrite"))
        .name("Bucket")
        .argument
        .isIdentifier
        .whereNot(_.code(".*tmp.*"))
        .l

    val bucketNames = bucketArgs
      .map(arg =>
        cpg
          .call(Operators.assignment)
          .where(_.astChildren.isIdentifier.name(arg.name))
          .argument
          .argumentIndex(2)
          .isLiteral
          .code
          .l
      )
      .l
      .flatten
      .distinct

    readWriteBucketNames ++= bucketNames
      .map(bucket => {
        val bucketName =
          bucket.stripPrefix("\"").stripSuffix("\"").stripPrefix("\'").stripSuffix("\'").stripPrefix("s3a://")
        if bucketName.matches(bucketPattern) then bucketName
        else ""
      })
      .filter(_.nonEmpty)
      .l

    // add S3 details to DatabaseDetails
    addReadWriteS3Details(readWriteBucketNames, s3DatabaseDetailsCache)
  }

}
