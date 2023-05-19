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
 *
 */

package ai.privado.tagger.collection

import ai.privado.cache.RuleCache
import ai.privado.model.{Constants, RuleInfo}
import ai.privado.tagger.PrivadoParallelCpgPass
import ai.privado.utility.Utilities._
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.slf4j.LoggerFactory

class WebFormsCollectionTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoParallelCpgPass[RuleInfo](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[RuleInfo] =
    ruleCache.getRule.collections.filter(_.catLevelTwo == "webforms").toArray

  override def runOnPart(builder: DiffGraphBuilder, collectionRuleInfo: RuleInfo): Unit = {

    /** return ( <form onSubmit={handleSubmit}> <TextBox label="Name" type="text" name="name" onChange={handleChange} />
      * <TextBox label="Email" type="email" name="email" onChange={handleChange} /> <TextBox label="Message" multiline
      * rows={4} name="message" onChange={handleChange} /> <Button type="submit">Submit</Button> </form> );
      */
    ruleCache.getRule.sources.foreach(sourceRule => {
      val rule =
        s"${collectionRuleInfo.combinedRulePattern}.*(?:name|id|label|value)=(?:\\{|t){0,2}(?:\"|\'|`)(${sourceRule.combinedRulePattern})(?:\"|\'|`).*"
      cpg.templateDom
        // Each HTML element/template element translates into multiple CPG nodes.
        // Like
        // One node representing the entire HTML tag/element with everything included in starting and ending tag - name => JSXElement.
        // One node representing starting / opening tag of the element - name => JSXOpeningElement. Similarly, there is a node for the ending tag - name => JSXClosingElement
        // One node representing each attribute of the element - name => JSXAttribute
        // There are more such nodes. I have listed only a few which are important for the below logic.
        //
        // As we cannot tag the same CPG node with multiple rules along with its metadata. As we cannot group those tags, in turn, results in an ambiguous state.
        // In order to tackle this situation. I decided to use JSXOpeningElement to tag the collection rule (privado.json -> collection section)
        // and JSXElement to tag source rule information (privado.json -> source and processing section)
        //
        .name(
          s"${Constants.jsxOpenElement}|${Constants.jsxElement}|${Constants.HTMLOpenElement}|${Constants.HTMLElement}"
        )
        .code(rule)
        .foreach(element => {
          if (element.name == Constants.jsxOpenElement || element.name == Constants.HTMLOpenElement) {
            storeForTag(builder, element, ruleCache)(Constants.collectionSource, sourceRule.id)
            addRuleTags(builder, element, collectionRuleInfo, ruleCache)
          } else if (element.name == Constants.jsxElement || element.name == Constants.HTMLElement) {
            addRuleTags(builder, element, sourceRule, ruleCache)
          }
        })
    })
  }
}
