/*
 * This file is part of Privado OSS.
 *
 * Privado is an open source static code analysis tool to discover data flows in the code.
 * Copyright (C) 2022 Privado, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * For more information, contact support@privado.ai
 */

package ai.privado.java.exporter

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
