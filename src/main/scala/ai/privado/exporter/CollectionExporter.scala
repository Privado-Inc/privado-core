package ai.privado.exporter

import ai.privado.model.{Constants, NodeType}
import io.circe.Json
import io.circe.syntax.EncoderOps
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method
import io.shiftleft.semanticcpg.language._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.Success

class CollectionExporter(cpg: Cpg) {

  /*
    Processes collection points and return final output
   */
  def getCollections = {
    val collectionMapByCollectionId = cpg.method
      .where(_.tag.nameExact(Constants.nodeType).valueExact(NodeType.COLLECTIONS.toString))
      .l
      .groupBy(collectionMethod => collectionMethod.tag.nameExact(Constants.id).value.head)

    collectionMapByCollectionId.foreach(id => {
      println(s"${id._1}, ${id._2.map(m => m.parameter.name.l)}")
    })

    collectionMapByCollectionId.map(entrySet => processByCollectionId(entrySet._1, entrySet._2))
  }

  private def processByCollectionId(collectionId: String, collectionMethods: List[Method]) = {

    val collectionParameterMapById = mutable.HashMap[String, ListBuffer[Method]]()

    collectionMethods.foreach(collectionMethod => {
      collectionMethod.parameter
        .or(_.tag.nameExact(Constants.id), _.tag.name(Constants.privadoDerived + ".*"))
        .foreach(parameter => {
          try {
            def addToMap(parameterId: String): Unit = {
              if (!collectionParameterMapById.contains(parameterId))
                collectionParameterMapById(parameterId) = ListBuffer()
              collectionParameterMapById(parameterId).append(collectionMethod)
            }
            val parameterList = parameter.tag.nameExact(Constants.id).value.l
            if (parameterList.nonEmpty)
              addToMap(parameterList.head)
            else
              parameter.tag.name(Constants.privadoDerived + ".*").value.foreach(addToMap)

          } catch {
            case e: Exception => println(s"parameter got fUP : ${e.toString}")
          }
        })
    })

    val collectionOutput = mutable.LinkedHashMap[String, Json]()
    collectionOutput.addOne("collectionId" -> collectionId.asJson)

    collectionOutput.addOne(
      "collections" -> collectionParameterMapById
        .map(entrySet => processByParameterId(entrySet._1, entrySet._2.toList))
        .asJson
    )

    collectionOutput
  }

  def processByParameterId(parameterId: String, methodOccurrences: List[Method]) = {

    val parameterCollectionOutput = mutable.LinkedHashMap[String, Json]()
    parameterCollectionOutput.addOne(Constants.sourceId -> parameterId.asJson)
    parameterCollectionOutput.addOne(
      Constants.occurrences -> ExporterUtility.convertPathElement(methodOccurrences).asJson
    )
    parameterCollectionOutput
  }

}
