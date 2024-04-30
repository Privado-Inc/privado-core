package ai.privado.entrypoint

import ai.privado.utility.StatsRecorder

import java.io.File
import scala.collection.mutable.Map

trait GeneralMetadataLoggers {

  def logRepositoryFiledata(repoPath: String, statsRecorder: StatsRecorder): Unit = {
    val extensionCount    = Map[String, Int]()
    val folderSizeInBytes = getSizeAndExtensionCount(File(repoPath), extensionCount)
    statsRecorder.justLogMessage(
      s"Repository folder size in bytes -> $folderSizeInBytes -> ${formatSize(folderSizeInBytes)}"
    )
    extensionCount.toList.sortBy(_._2).reverse.foreach { case (extension, count) =>
      statsRecorder.justLogMessage(s"%-${15}s%s".format(s"'$extension'", s" -> count '$count'"))
    }
  }

  private def getSizeAndExtensionCount(fileOrFolder: File, extensionCount: Map[String, Int]): Long = {
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
      val name = fileOrFolder.getName
      if (name.contains('.')) {
        extensionCount.updateWith(name.substring(name.lastIndexOf('.'))) {
          case Some(count) => Some(count + 1)
          case None        => Some(1)
        }
      } else {
        extensionCount.updateWith("unknown") {
          case Some(count) => Some(count + 1)
          case None        => Some(1)
        }
      }
      fileOrFolder.length
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
