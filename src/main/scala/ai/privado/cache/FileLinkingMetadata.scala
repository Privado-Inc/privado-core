package ai.privado.cache

import scala.collection.mutable
class FileLinkingMetadata {

  private val dataflowMap = mutable.HashMap[String, mutable.HashSet[String]]()

  /** Given dataflow file paths, the function calculate all permutations and stores it in dataflowMap
    *
    * For input List(List(a,b,c), List(c,d)) we will store
    *
    * a -> (a,b,c)
    *
    * b -> (a,b,c)
    *
    * c -> (a,b,c,d)
    *
    * d -> (c,d)
    *
    * @param dataflowFiles
    */
  def addToDataflowMap(dataflowFiles: List[List[String]]): Unit = {

    val pairs = for {
      sublist <- dataflowFiles
      elem    <- sublist
    } yield (elem, sublist)

    /* Explaining the above piece of code
    dataflow files => List(List(a,b,c), List(c,d))
    pairs => List((a, List(a,b,c)), (b, List(a,b,c)), (c, List(a,b,c)), (c, List(c,d), (d, List(c,d)))
     */
    val grouped = pairs.groupBy(_._1)

    grouped.foreach { case (key, valuePairs) =>
      if (!dataflowMap.contains(key))
        dataflowMap(key) = mutable.HashSet[String]()
      dataflowMap(key).addAll(valuePairs.flatMap(_._2).distinct)
    }
    /*
    Here we will have dataflowMap as
    a -> (a,b,c)
    b -> (a,b,c)
    c -> (a,b,c,d)
    d -> (c,d)
     */
  }

  def getDataflowMap: Map[String, mutable.HashSet[String]] = this.dataflowMap.toMap

}
