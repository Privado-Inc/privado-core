package ai.privado.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.*
import ai.privado.passes.FileExtensions
import ai.privado.semantic.Language.*
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier}
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

import scala.collection.mutable.{HashMap, ListBuffer}
import scala.xml.XML

class AndroidCollectionTagger(cpg: Cpg, projectRoot: String, ruleCache: RuleCache)
    extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[RuleInfo] =
    ruleCache.getRule.collections.filter(_.catLevelTwo == Constants.android).toArray

  override def runOnPart(builder: DiffGraphBuilder, collectionRuleInfo: RuleInfo): Unit = {
    /* We have all the XML layout nodes in cpg.androidXmlLayoutNodes containing
     * parsed id (eg. `emailEditText`), node type (`<ExitText>`), line and col number. We now
     * find all points in code where the id is used and tag those nodes as collection points.
     *
     * For example, in code, we would have something like this:
     *
     *   (1) When binding is used:
     *
     *       binding.emailEditText.afterTextChanged { email ->
     *              viewModel.isEmailFormatCorrect(email).observe(this, Observer
     *              ....
     *
     *   (2) When findViewById is used:
     *
     *       val myText = findViewById<EditText>(R.id.emailEditText)
     *
     * Before tagging however, we have to filter only those nodes which match any source PII rule
     * */

    // For case (1), we want to tag the `fieldIdentifiers` here (`binding.emailEditText`).
    val fieldIdentifiers = cpg.androidXmlLayoutNode
      .name(collectionRuleInfo.combinedRulePattern)
      .flatMap { elem =>
        cpg.fieldAccess.astChildren.isFieldIdentifier.where(_.canonicalName(elem.name)).l
      }
      .toList
    if (fieldIdentifiers.nonEmpty) {
      fieldIdentifiers.foreach(node => {
        storeForTag(builder, node, ruleCache)(Constants.collectionSource, node.canonicalName)
        addRuleTags(builder, node, collectionRuleInfo, ruleCache)
      })
    }
    // TODO: for case (2), fieldIdentifier approach works for now, but we should ideally tag findViewById
  }

}
