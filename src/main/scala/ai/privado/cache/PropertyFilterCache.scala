package ai.privado.cache

import ai.privado.model.Constants
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

import scala.collection.mutable

class PropertyFilterCache {

  private val fileSkippedBySizeMap: mutable.Map[String, String] = new mutable.HashMap[String, String]()

  private val fileSkippedByDirCountMap: mutable.Map[String, List[String]] = new mutable.HashMap[String, List[String]]()

  def addIntoFileSkippedBySize(filePath: String, fileSize: Int): Unit = {
    fileSkippedBySizeMap.put(filePath, fileSize.toString)
  }

  def addIntoFileSkippedByDirCount(dirPath: String, propertiesPath: List[String]): Unit = {
    fileSkippedByDirCountMap.put(dirPath, propertiesPath)
  }

  def getFileSkippedBySizeData(ruleCache: RuleCache): FileSkippedBySizeModel = {
    val skippedSizeLimit = ruleCache.getSystemConfigByKey(Constants.PropertyFileSizeLimit, true).toString
    val skippedFileList = fileSkippedBySizeMap.map((path, size) => {
      FileSkippedBySizeListModel(path, s"$size KB")
    })
    FileSkippedBySizeModel(skippedSizeLimit, skippedFileList.toList)
  }

  def getFileSkippedDirCountData(ruleCache: RuleCache): FileSkippedByDirCountModel = {
    val dirCountLimit = ruleCache.getSystemConfigByKey(Constants.PropertyFileDirCountLimit, true)
    val skippedFirFileList = fileSkippedByDirCountMap.map { case (dirPath: String, fileList: List[String]) =>
      FileSkippedByDirCountListModel(dirPath, fileList.size.toString, fileList)
    }.toList
    FileSkippedByDirCountModel(dirCountLimit, skippedFirFileList)
  }
}

case class FileSkippedBySizeModel(currentFileSizeLimit: String, skipLists: List[FileSkippedBySizeListModel])

case class FileSkippedBySizeListModel(file: String, size: String)

case class FileSkippedByDirCountModel(currentFilesInDirLimit: String, skipList: List[FileSkippedByDirCountListModel])

case class FileSkippedByDirCountListModel(dir: String, countOfPropertyFiles: String, files: List[String])

object PropertyFilterCacheEncoderDecoder {
  implicit val FileSkippedBySizeModelDecoder: Decoder[FileSkippedBySizeModel] =
    deriveDecoder[FileSkippedBySizeModel]

  implicit val fileSkippedBySizeModelEncoder: Encoder[FileSkippedBySizeModel] =
    deriveEncoder[FileSkippedBySizeModel]

  implicit val fileSkippedBySizeListModelDecoder: Decoder[FileSkippedBySizeListModel] =
    deriveDecoder[FileSkippedBySizeListModel]

  implicit val fileSkippedBySizeListModelEncoder: Encoder[FileSkippedBySizeListModel] =
    deriveEncoder[FileSkippedBySizeListModel]

  implicit val fileSkippedByDirCountModelDecoder: Decoder[FileSkippedByDirCountModel] =
    deriveDecoder[FileSkippedByDirCountModel]

  implicit val fileSkippedByDirCountModelEncoder: Encoder[FileSkippedByDirCountModel] =
    deriveEncoder[FileSkippedByDirCountModel]

  implicit val fileSkippedByDirCountListModelDecoder: Decoder[FileSkippedByDirCountListModel] =
    deriveDecoder[FileSkippedByDirCountListModel]

  implicit val fileSkippedByDirCountListModelEncoder: Encoder[FileSkippedByDirCountListModel] =
    deriveEncoder[FileSkippedByDirCountListModel]
}
