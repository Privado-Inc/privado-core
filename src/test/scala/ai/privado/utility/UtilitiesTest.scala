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

package ai.privado.utility

import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class UtilitiesTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  "getDomainFromTemplates test" should {
    "domain extratcion sample one" in {
      val code =
        """<Script
      id="amazon-tag"
      data-testid="amazon-tag"
      src="https://c.amazon-adsystem.com/aax2/apstag.js"
      strategy="lazyOnload"
    />"""
      val domain = Utilities.getDomainFromTemplates(code)
      domain shouldBe "c.amazon-adsystem.com"
    }
    "domain extratcion sample two" in {
      val code =
        """<Script
        id="googletag-script"
        data-testid="googletag-script"
        src="https://www.googletagservices.com/tag/js/gpt.js"
        strategy="lazyOnload"
      />"""
      val domain = Utilities.getDomainFromTemplates(code)
      domain shouldBe "googletagservices.com"
    }
    "domain extratcion sample three" in {
      val code =
        """<Script
    id="googletag-script"
    data-testid="googletag-script"
    src="//www.googletagservices.com/tag/js/gpt.js"
    strategy="lazyOnload"
  />"""
      val domain = Utilities.getDomainFromTemplates(code)
      domain shouldBe "googletagservices.com"
    }

    "domain extratcion sample four" in {
      val code =
        """<Script
        id="prebid-script"
        data-testid="prebid-script"
        src={`${CDN_ADS_URL}/prebid.js`}
        strategy="lazyOnload"
        />"""
      val domain = Utilities.getDomainFromTemplates(code)
      domain shouldBe "unknown-domain"
    }
  }
}
