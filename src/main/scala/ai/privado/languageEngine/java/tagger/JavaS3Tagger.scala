package ai.privado.languageEngine.java.tagger

import ai.privado.tagger.S3BaseTagger
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

class JavaS3Tagger(cpg: Cpg, s3DatabaseDetailsCache: S3DatabaseDetailsCache)
    extends S3BaseTagger(cpg, s3DatabaseDetailsCache) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def run(builder: DiffGraphBuilder): Unit = {
    val readBucketNames  = ListBuffer[String]()
    val writeBucketNames = ListBuffer[String]()

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

    // add S3 details to DatabaseDetails
    addReadS3Details(readBucketNames, s3DatabaseDetailsCache)
    addWriteS3Details(writeBucketNames, s3DatabaseDetailsCache)

  }
}
