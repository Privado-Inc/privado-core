package ai.privado.passes

import ai.privado.testfixtures.DefaultFrontendTestSuite
import io.shiftleft.semanticcpg.language.*

class HighTouchPassTests extends DefaultFrontendTestSuite {
  "hightouch pass" should {
    val cpg = code(
      """
        |sources:
        |  snowflake:
        |    name: Snowflake - EXT_HIGHTOUCH
        |    type: snowflake
        |destinations:
        |  the-trade-desk-wave-maker:
        |    name: The Trade Desk - WaveMaker
        |    type: tradedesk
        |  tik-tok-ads:
        |    name: TikTok Ads
        |    type: tiktok
        |  snapchat:
        |    name: Snapchat
        |    type: snapchat
        |  google-ads:
        |    name: Google Ads
        |    type: google
        |  facebook-custom-audiences:
        |    name: Facebook Custom Audiences
        |    type: facebook
        |  habu-amc-s3:
        |    name: Habu AMC S3
        |    type: s3
        |  the-trade-desk:
        |    name: The Trade Desk
        |    type: tradedesk
        |""".stripMargin,
      "manifest.yaml"
    )
      .moreCode(
        """
        |model-slug: some-unique-id
        |name: FPN Push Sends (% 4 = 2)
        |source: snowflake-service-hightouch-outbound
        |description: Group 3 (USER_ENTITY_ID % 4 = 2)
        |type: raw_sql
        |rawSql: |-
        |  SELECT
        |    USER_ENTITY_ID,
        |    MAX(IGUAZU_SENT_AT) AS FpnPushSentAt
        |  FROM
        |    OUTBOUND.NOTIFICATION_PLATFORM_ANALYTICS_EVENT_V
        |  WHERE
        |    channel = 'CHANNEL_TYPE_PUSH'
        |    AND IGUAZU_SENT_AT >= DATEADD(MINUTE, -30, GETDATE())
        |    AND USER_ENTITY_ID % 4 = 2
        |  GROUP BY
        |    USER_ENTITY_ID
        |primaryKey: USER_ENTITY_ID
        |""".stripMargin,
        "model.yaml"
      )
      .moreCode(
        """
        |model: dx-top-of-funnel-waitlist
        |destination: braze-dx
        |description: >-
        |  https://doordash.atlassian.net/browse/CRMO-1152 ,
        |  https://doordash.atlassian.net/browse/CRMO-1023
        |config:
        |  mode: update
        |  type: object
        |  object: user
        |  mappings: []
        |  aliasMappings: []
        |  configVersion: 2
        |  customMappings:
        |    - to: ht_tof_waitlisted_nested
        |      from: HT_TOF_WAITLIST_NESTED
        |      type: standard
        |  externalIdMapping:
        |    to: external_id
        |    from: EMAIL
        |    type: standard
        |schedule:
        |  type: interval
        |  schedule:
        |    interval:
        |      unit: hour
        |      quantity: 24
        |schedulePaused: false
        |""".stripMargin,
        "sync.yaml"
      )

    println(cpg.call.l)
  }
}
