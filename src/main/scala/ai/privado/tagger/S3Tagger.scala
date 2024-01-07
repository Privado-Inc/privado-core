package ai.privado.tagger

import ai.privado.cache.{DatabaseDetailsCache, S3DatabaseDetailsCache}
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.DatabaseDetails
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages, Operators}
import org.slf4j.LoggerFactory
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import org.slf4j.LoggerFactory
import overflowdb.traversal.*

import scala.collection.mutable.ListBuffer

class S3Tagger(cpg: Cpg) extends PrivadoSimpleCpgPass(cpg) {

  private val logger          = LoggerFactory.getLogger(getClass)
  private val bucketPattern   = "(?!(^xn--|.+-s3alias$))^[a-z0-9][a-z0-9-]{1,61}[a-z0-9]$"
  private val readRuleId      = "Storages.AmazonS3.Read"
  private val writeRuleId     = "Storages.AmazonS3.Write"
  private val readWriteRuleId = "Storages.AmazonS3.ReadAndWrite"

  override def run(builder: DiffGraphBuilder): Unit = {
    val readBucketNames      = ListBuffer[String]()
    val writeBucketNames     = ListBuffer[String]()
    val readWriteBucketNames = ListBuffer[String]()

    if (cpg.metaData.language.head == Languages.JAVASRC) {

      // find all S3 Read tagged locations and extract bucket names
      readBucketNames ++= cpg.call
        .where(_.tag.valueExact("Storages.AmazonS3.Read"))
        .repeat(_.astParent)(_.emit(_.isCall).maxDepth(4))
        .isCallTo("bucket")
        .argument(1)
        .isLiteral
        .map(bucket => {
          val bucketName = bucket.code.stripPrefix("\"").stripSuffix("\"")
          if bucketName.matches(bucketPattern) then bucketName
          else ""
        })
        .filter(_.nonEmpty)
        .l

      // find all S3 Write tagged locations and extract bucket names
      writeBucketNames ++= cpg.call
        .where(_.tag.valueExact("Storages.AmazonS3.Write"))
        .repeat(_.astParent)(_.emit(_.isCall).maxDepth(4))
        .isCallTo("bucket")
        .argument(1)
        .isLiteral
        .map(bucket => {
          val bucketName = bucket.code.stripPrefix("\"").stripSuffix("\"")
          if bucketName.matches(bucketPattern) then bucketName
          else ""
        })
        .filter(_.nonEmpty)
        .l

    } else if (cpg.metaData.language.head == Languages.PYTHONSRC) {
      // handle case where bucket names are assigned to identifier args having some "bucket" as name, eg.
      //    s3_client.put_object(Body = pdf_result, Bucket = s3_bucket, Key = s3_filename)
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
    }

    // remove existing DB details from rule and add READ DB DETAILS to separate S3 DB details
    DatabaseDetailsCache.removeDatabaseDetails(readRuleId)
    S3DatabaseDetailsCache.addS3DatabaseDetails(
      readBucketNames.toList.map(bucketName => DatabaseDetails(bucketName, "Amazon S3", "amazon.com", "Read", "")),
      readRuleId
    )

    // remove existing DB details from rule and add WRITE DETAILS to separate S3 DB details
    DatabaseDetailsCache.removeDatabaseDetails(writeRuleId)
    S3DatabaseDetailsCache.addS3DatabaseDetails(
      writeBucketNames.toList.map(bucketName => DatabaseDetails(bucketName, "Amazon S3", "amazon.com", "Write", "")),
      writeRuleId
    )

    // remove existing DB details from rule and add READ-WRITE DETAILS to separate S3 DB details
    DatabaseDetailsCache.removeDatabaseDetails(readWriteRuleId)
    S3DatabaseDetailsCache.addS3DatabaseDetails(
      readWriteBucketNames.toList.map(bucketName =>
        DatabaseDetails(bucketName, "Amazon S3", "amazon.com", "Read/Write", "")
      ),
      readWriteRuleId
    )
  }

}
