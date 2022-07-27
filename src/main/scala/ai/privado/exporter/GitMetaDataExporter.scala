package ai.privado.exporter

import ai.privado.model.Constants

import scala.collection.mutable
import better.files.File

import scala.util.control.Breaks.{break, breakable}
import scala.util.{Success, Try}

object GitMetaDataExporter {

  def getMetaData(repoPath: String) = {
    val metaData = mutable.LinkedHashMap[String, String]()
    if (File(repoPath + "/.git").exists) {
      try {
        metaData.addOne(Constants.branchName -> getValue(getBranchName(repoPath)))
        metaData.addOne(Constants.commitId   -> getValue(getCommitId(repoPath)))
        metaData.addOne(Constants.remoteUrl  -> getValue(getRemoteUrl(repoPath)))
      } catch {
        case ex: Exception => println("Error while fetching git metadata")
      }
    }
    metaData
  }

  private def getBranchName(repoPath: String): Try[String] = Try {
    val f = File(repoPath + "/.git/HEAD")
    f.contentAsString.strip().split("heads/")(1)
  }

  private def getCommitId(repoPath: String) = Try {
    val f = File(repoPath + "/.git/logs/HEAD")
    f.contentAsString.strip().split(" ")(1)
  }

  private def getRemoteUrl(repoPath: String) = Try {
    val f         = File(repoPath + "/.git/config")
    var remoteUrl = ""
    breakable {
      for (line <- f.contentAsString.strip().split("\n")) {
        if (line.strip().startsWith("url =")) {
          remoteUrl = line.strip().split(" ").last
          break()
        }
      }
    }
    remoteUrl
  }

  private def getValue(value: Try[String]) = {
    value match {
      case Success(x) => x
      case _          => ""
    }
  }

}
