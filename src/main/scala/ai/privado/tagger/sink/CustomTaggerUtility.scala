package ai.privado.tagger.sink

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.TypeDecl
import io.shiftleft.semanticcpg.language._

object CustomTaggerUtility {

  /** Fetch all the typeDeclNode whose annotation name match the stringPattern regex
    *
    * @param stringPattern
    * @return
    */
  def getImpactedTypeDeclNodeByAnnotation(cpg: Cpg, stringPattern: String): List[TypeDecl] = {
    if (stringPattern.nonEmpty && !stringPattern.equals("()")) {
      cpg.typeDecl.where(_.annotation.name(stringPattern)).l
    } else {
      List()
    }

  }

  /** Fetch all the typeDeclNode which inherits from classes which match the stringPattern regex
    *
    * @param stringPattern
    * @return
    */
  def getImpactedTypeDeclNodeByExtends(cpg: Cpg, stringPattern: String): List[TypeDecl] = {
    if (stringPattern.nonEmpty && !stringPattern.equals("()")) {
      cpg.typeDecl
        .filter(
          _.inheritsFromTypeFullName
            .map(inheritsFrom => {
              inheritsFrom.matches(stringPattern)
            })
            .foldLeft(false)((a, b) => a || b)
        )
        .l
    } else {
      List()
    }

  }

}
