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

package ai.privado.languageEngine.java.tagger.sink

import ai.privado.cache.RuleCache
import ai.privado.model.Constants
import ai.privado.tagger.PrivadoSimpleCpgPass
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import io.shiftleft.semanticcpg.language._
import ai.privado.utility.Utilities.addRuleTags

class MessagingConsumerCustomTagger(cpg: Cpg, ruleCache: RuleCache) extends PrivadoSimpleCpgPass(cpg) {
  override def run(builder: DiffGraphBuilder): Unit = {

    // JMS Rule Tagging
    ruleCache.getRuleInfo(Constants.jmsConsumerRuleId) match {
      case Some(jmsRule) =>
        /*
        Fetching and tagging cases like
        class RecieverMDB extends MessageListener {
        public void onMessage(Message message) {}
        }
         */
        cpg.typeDecl
          .filter(_.inheritsFromTypeFullName.contains("javax.jms.MessageListener"))
          .method
          .name("onMessage")
          .foreach(addRuleTags(builder, _, jmsRule, ruleCache))

        /*
        To tag methods which have Jmslistener annotation
         */
        cpg.annotation.name("JmsListener").method.foreach(addRuleTags(builder, _, jmsRule, ruleCache))
      case None =>
    }

    // Kafka Rule Tagging
    ruleCache.getRuleInfo(Constants.kafkaConsumerRuleId) match {
      case Some(kafkaRule) =>
        cpg.annotation.name("KafkaListener").method.foreach(addRuleTags(builder, _, kafkaRule, ruleCache))
      case None =>
    }

  }
}
