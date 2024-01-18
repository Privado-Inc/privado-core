package ai.privado.tagger

import ai.privado.cache.{DatabaseDetailsCache, S3DatabaseDetailsCache}
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.DatabaseDetails
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages, Operators}
import org.slf4j.LoggerFactory
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Method}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.nodes.JavaProperty
import overflowdb.traversal.*

import scala.collection.mutable.ListBuffer

abstract class S3BaseTagger(cpg: Cpg) extends PrivadoSimpleCpgPass(cpg) {

  val logger          = LoggerFactory.getLogger(getClass)
  val bucketPattern   = "(?!(^xn--|.+-s3alias$))^[a-z0-9][a-z0-9-]{1,61}[a-z0-9]$"
  val readRuleId      = "Storages.AmazonS3.Read"
  val writeRuleId     = "Storages.AmazonS3.Write"
  val readWriteRuleId = "Storages.AmazonS3.ReadAndWrite"

  def addReadS3Details(readBucketNames: ListBuffer[String]): Unit = {
    // remove existing DB details from rule and add READ DB DETAILS to separate S3 DB details
    DatabaseDetailsCache.removeDatabaseDetails(readRuleId)
    S3DatabaseDetailsCache.addS3DatabaseDetails(
      readBucketNames.toList.map(bucketName => DatabaseDetails(bucketName, "Amazon S3", "amazon.com", "Read", "")),
      readRuleId
    )
  }

  def addWriteS3Details(writeBucketNames: ListBuffer[String]): Unit = {
    // remove existing DB details from rule and add WRITE DETAILS to separate S3 DB details
    DatabaseDetailsCache.removeDatabaseDetails(writeRuleId)
    S3DatabaseDetailsCache.addS3DatabaseDetails(
      writeBucketNames.toList.map(bucketName => DatabaseDetails(bucketName, "Amazon S3", "amazon.com", "Write", "")),
      writeRuleId
    )
  }

  def addReadWriteS3Details(readWriteBucketNames: ListBuffer[String]): Unit = {
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
