package ai.privado.exporter

import ai.privado.cache.RuleCache

import scala.collection.mutable
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import ai.privado.semantic.language.*
import ai.privado.model.Constants

import scala.collection.immutable.HashMap

object RepoConfigMetaDataExporter {

  def getMetaData(cpg: Cpg, ruleCache: RuleCache) = {
    val configRule = ruleCache.getSystemConfigByKey(Constants.RepoPropertyConfig)
    try {
      cpg.property
        .filter(p => p.name matches configRule)
        .l
        .map(p => {
          HashMap(
            Constants.name     -> p.name,
            Constants.value    -> p.value,
            Constants.filePath -> p.file.name.headOption.getOrElse("")
          )
        })
    } catch {
      case ex: Exception => {
        println("Error while fetching repo config metadata")
        List()
      }
    }
  }
}
