package ai.privado.tagger

import ai.privado.cache.{DatabaseDetailsCache, S3DatabaseDetailsCache}
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.DatabaseDetails
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import org.slf4j.LoggerFactory
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import org.slf4j.LoggerFactory
import overflowdb.traversal.*

class S3Tagger(cpg: Cpg) extends PrivadoSimpleCpgPass(cpg) {

  private val logger        = LoggerFactory.getLogger(getClass)
  private val bucketPattern = "(?!(^xn--|.+-s3alias$))^[a-z0-9][a-z0-9-]{1,61}[a-z0-9]$"

  override def run(builder: DiffGraphBuilder): Unit = {
    if (cpg.metaData.language.head == Languages.JAVASRC) {
      // find all S3 Read tagged locations and extract bucket names
      val readBucketNames = cpg.call
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
      val writeBucketNames = cpg.call
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

      // remove existing DB details from rule and add to separate S3 DB details
      val readRuleId = "Storages.AmazonS3.Read"
      DatabaseDetailsCache.removeDatabaseDetails(readRuleId)
      S3DatabaseDetailsCache.addS3DatabaseDetails(
        readBucketNames.map(bucketName => DatabaseDetails("S3 Bucket", "Amazon", bucketName, "Read", "")),
        readRuleId
      )

      // remove existing DB details from rule and add to separate S3 DB details
      val writeRuleId = "Storages.AmazonS3.Write"
      DatabaseDetailsCache.removeDatabaseDetails(writeRuleId)
      S3DatabaseDetailsCache.addS3DatabaseDetails(
        writeBucketNames.map(bucketName => DatabaseDetails("S3 Bucket", "Amazon", bucketName, "Write", "")),
        writeRuleId
      )
    }
  }

}
