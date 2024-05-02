package ai.privado.entrypoint

import ai.privado.utility.StatsRecorder

import java.io.File
import scala.collection.mutable.Map

trait GeneralMetadataLoggers {

  def logRepositoryFiledata(repoPath: String, statsRecorder: StatsRecorder): Unit = {
    val extensionCount    = Map[String, ExtensionFileMetaData]()
    val folderSizeInBytes = getSizeAndExtensionCount(File(repoPath), extensionCount)
    statsRecorder.justLogMessage(
      s"Repository folder size in bytes -> $folderSizeInBytes -> ${formatSize(folderSizeInBytes)}"
    )
    extensionCount.toList.sortBy(_._2.count).reverse.foreach { case (extension, count) =>
      statsRecorder.justLogMessage(
        s"%-${15}s%s".format(s"'$extension'", s" -> count '${count.count}' -> size -> '${formatSize(count.size)}' ")
      )
    }
  }

  private def getSizeAndExtensionCount(fileOrFolder: File, extensionCount: Map[String, ExtensionFileMetaData]): Long = {
    if (fileOrFolder.getName.startsWith("."))
      return 0L
    if (fileOrFolder.isDirectory) {
      val files = fileOrFolder.listFiles
      if (files != null) {
        files.map(file => getSizeAndExtensionCount(file, extensionCount)).sum
      } else {
        0L
      }
    } else {
      val size = fileOrFolder.length
      val name = fileOrFolder.getName
      if (name.contains('.')) {
        extensionCount.updateWith(name.substring(name.lastIndexOf('.'))) {
          case Some(count) => Some(count.addFileCountAndSize(size))
          case None        => Some(ExtensionFileMetaData(1, size))
        }
      } else {
        extensionCount.updateWith("unknown") {
          case Some(count) => Some(count.addFileCountAndSize(size))
          case None        => Some(ExtensionFileMetaData(1, size))
        }
      }
      size
    }
  }

  private def formatSize(size: Long): String = {
    val kb = size / 1024
    val mb = kb / 1024
    val gb = mb / 1024
    if (gb > 0) {
      s"${gb}GB"
    } else if (mb > 0) {
      s"${mb}MB"
    } else if (kb > 0) {
      s"${kb}KB"
    } else {
      s"${size} bytes"
    }
  }
}

class ExtensionFileMetaData(var count: Int, var size: Long) {
  def addFileCountAndSize(fileSize: Long): ExtensionFileMetaData = {
    count += 1
    size += fileSize
    this
  }
}
