package ai.privado.exporter

import ai.privado.cache.RuleCache

import scala.collection.mutable
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.Constants

object RepoConfigMetaDataExporter {

  def getMetaData(cpg: Cpg, ruleCache: RuleCache) = {
    val metaData   = mutable.LinkedHashMap[String, String]()
    val configRule = ruleCache.getSystemConfigByKey(Constants.RepoPropertyConfig)
    try {
      val propertySources = cpg.property.filter(p => p.name matches configRule).l
      propertySources.foreach(p => {
        metaData.put(p.name, p.value)
      })
    } catch {
      case ex: Exception => println("Error while fetching repo config metadata")
    }
    metaData
  }
}