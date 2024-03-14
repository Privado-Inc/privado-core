package ai.privado.exporter

import ai.privado.cache.RuleCache

import scala.collection.mutable
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import ai.privado.languageEngine.java.language.NodeStarters
import ai.privado.model.Constants

object RepoConfigMetaDataExporter {

  def getMetaData(cpg: Cpg, ruleCache: RuleCache) = {
    val metaData   = mutable.LinkedHashSet[mutable.HashMap[String, String]]()
    val configRule = ruleCache.getSystemConfigByKey(Constants.RepoPropertyConfig)
    try {
      val propertySources = cpg.property.filter(p => p.name matches configRule).l
      propertySources.foreach(p => {
        val configMap = mutable.HashMap[String, String]()
        configMap.put(Constants.name, p.name)
        configMap.put(Constants.value, p.value)
        configMap.put(Constants.filePath, p.file.head.name)
        metaData.addOne(configMap)
      })
    } catch {
      case ex: Exception => println("Error while fetching repo config metadata")
    }
    metaData
  }
}
