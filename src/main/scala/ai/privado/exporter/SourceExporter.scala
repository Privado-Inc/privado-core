package ai.privado.exporter

import ai.privado.model.{CatLevelOne, Constants, InternalTag}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{CfgNode, Tag}
import io.shiftleft.semanticcpg.language._
import io.circe._
import io.circe.syntax._

import scala.collection.mutable
import scala.collection.mutable.{HashMap, LinkedHashMap}
import ExporterUtility._
import ai.privado.cache.RuleCache

class SourceExporter(cpg: Cpg) {

  lazy val sourcesTagList = getSourcesTagList
  lazy val sourcesList    = getSourcesList

  implicit val finder: NodeExtensionFinder = DefaultNodeExtensionFinder
  /*
    Fetch and Convert sources to desired output
   */
  def getSources = {
    convertSourcesList(sourcesTagList)
  }

  def getProcessing = {
    val processingMap = HashMap[String, List[CfgNode]]()
    sourcesList.foreach(source => {
      def addToMap(sourceId: String): Unit = {
        if (processingMap.contains(sourceId)) {
          processingMap(sourceId) = processingMap(sourceId).+:(source)
        } else {
          processingMap.addOne(sourceId -> List(source))
        }
      }
      if (source.tag.nameExact(Constants.catLevelOne).value.head.equals(CatLevelOne.SOURCES.name)) {
        addToMap(source.tag.nameExact(Constants.id).l.head.value)
      } else {
        source.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)
      }
    })
    processingMap.map(entrySet =>
      mutable.LinkedHashMap[String, Json](
        Constants.sourceId    -> entrySet._1.asJson,
        Constants.occurrences -> ExporterUtility.convertPathElement(entrySet._2).asJson
      )
    )
  }

  /*
    Fetch all the sources tag
   */
  private def getSourcesTagList = {
    val sources =
      cpg.identifier
        .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SOURCES.name))
        .map(item => item.tag.l)
        .l ++
        cpg.literal
          .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SOURCES.name))
          .map(item => item.tag.l)
          .l ++
        cpg.call
          .where(_.tag.nameExact(Constants.catLevelOne).valueExact(CatLevelOne.SOURCES.name))
          .map(item => item.tag.l)
          .l
    sources
  }

  /*
    Fetch all the sources node
   */
  private def getSourcesList: List[CfgNode] = {
    val sources =
      cpg.identifier
        .where(
          _.tag
            .nameExact(Constants.catLevelOne)
            .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
        )
        .l ++
        cpg.literal
          .where(
            _.tag
              .nameExact(Constants.catLevelOne)
              .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
          )
          .l ++
        cpg.call
          .where(
            _.tag
              .nameExact(Constants.catLevelOne)
              .or(_.valueExact(CatLevelOne.SOURCES.name), _.valueExact(CatLevelOne.DERIVED_SOURCES.name))
          )
          .l
    sources
  }

  private def convertSourcesList(sources: List[List[Tag]]) = {
    def convertToStandardFormat(nodeList: List[Tag]) = {
      val orderedSourceMap = new LinkedHashMap[String, Json]()
      val node = nodeList
        .filterNot(node => InternalTag.valuesAsString.contains(node.name))
        .filter(node => node.name.equals(Constants.id))
      if (node.nonEmpty) {
        val ruleId = node.head.value
        orderedSourceMap.addOne(Constants.sourceType -> {
          RuleCache.getRuleInfo(ruleId) match {
            case Some(rule) => rule.catLevelOne.label.asJson
            case None       => "".asJson
          }
        })
        orderedSourceMap ++ ExporterUtility.getRuleInfoForExporting(ruleId)
      } else
        orderedSourceMap
    }
    sources.map(source => convertToStandardFormat(source)).toSet
  }

}
