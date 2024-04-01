package ai.privado.cache

import ai.privado.model.Constants

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
    val skippedSizeLimit = ruleCache.getSystemConfigByKey(Constants.PropertyFileSizeLimit).toString
    val skippedFileList = fileSkippedBySizeMap.map((path, size) => {
      FileSkippedBySizeListModel(path, s"$size KB")
    })
    FileSkippedBySizeModel(skippedSizeLimit, skippedFileList.toList)
  }

  def getFileSkippedDirCountData(ruleCache: RuleCache): FileSkippedByDirCountModel = {
    val dirCountLimit = ruleCache.getSystemConfigByKey(Constants.PropertyFileDirCountLimit)
    val skippedFirFileList = fileSkippedByDirCountMap.map((dirPath: String, fileList: List[String]) => {
      FileSkippedByDirCountListModel(dirPath, fileList.size.toString, fileList)
    })
    FileSkippedByDirCountModel(dirCountLimit, skippedFirFileList)
  }
}

case class FileSkippedBySizeModel(currentFileSizeLimit: String, skipLists: List[FileSkippedBySizeListModel])

case class FileSkippedBySizeListModel(file: String, size: String)

case class FileSkippedByDirCountModel(currentFilesInDirLimit: String, skipList: List[FileSkippedByDirCountModel])

case class FileSkippedByDirCountListModel(dir: String, countOfPropertyFiles: String, files: List[String])
