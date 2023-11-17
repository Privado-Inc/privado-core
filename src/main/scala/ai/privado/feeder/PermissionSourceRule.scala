package ai.privado.feeder

import ai.privado.feeder.MiniatureRuleModel

case class MiniatureRuleModel(id: String, pattern: String)
object PermissionSourceRule {

  val miniatureRuleList: List[MiniatureRuleModel] = List(
    MiniatureRuleModel(
      "Data.Sensitive.AudioVisualAndSensoryData.Video",
      "(android[.]permission[.]){0,1}(CAMERA|CAMERA_DISABLE_TRANSMIT_LED)"
    ),
    MiniatureRuleModel(
      "Data.Sensitive.LocationData.ApproximateLocation",
      "(android[.]permission[.]){0,1}(ACCESS_COARSE_LOCATION)"
    ),
    MiniatureRuleModel(
      "Data.Sensitive.LocationData.PreciseLocation",
      "(android[.]permission[.]){0,1}(ACCESS_BACKGROUND_LOCATION|ACCESS_FINE_LOCATION|ACCESS_MEDIA_LOCATION|ACCESS_LOCATION_EXTRA_COMMANDS|LOCATION_HARDWARE)"
    ),
    MiniatureRuleModel("<<<Calender>>", ""),
    MiniatureRuleModel(
      "Data.Sensitive.ContactData.PhoneNumber",
      "(android[.]permission[.]){0,1}(READ_CONTACTS|WRITE_CONTACTS|READ_PHONE_NUMBERS)"
    ),
    MiniatureRuleModel(
      "Data.Sensitive.UserContentData.EmailsorTextMessages",
      "(android[.]permission[.]){0,1}(READ_CALL_LOG|WRITE_CALL_LOG|USE_SIP|CALL_PHONE|ACCEPT_HANDOVER|ANSWER_PHONE_CALLS|PROCESS_OUTGOING_CALLS|READ_SMS|RECEIVE_SMS|SEND_SMS|RECEIVE_MMS|RECEIVE_WAP_PUSH)|(com[.]android[.]voicemail[.]permission[.]ADD_VOICEMAIL)"
    ),
    MiniatureRuleModel(
      "Data.Sensitive.AudioVisualAndSensoryData.AudioRecordings",
      "(android[.]permission[.]){0,1}(RECORD_AUDIO|MODIFY_AUDIO_SETTINGS)"
    ),
    MiniatureRuleModel("<<<STORAGE>>>", ""),
    MiniatureRuleModel(
      "Data.Sensitive.AudioVisualAndSensoryData.IoTorSensorData",
      "(android[.]permission[.]){0,1}(ACTIVITY_RECOGNITION|BODY_SENSORS)"
    ),
    MiniatureRuleModel("<<<BLUETOOTH>>>", ""),
    MiniatureRuleModel(
      "Data.Sensitive.AccountData.AccountID",
      "(android[.]permission[.]){0,1}(GET_ACCOUNTS|READ_PHONE_STATE)"
    ),
    MiniatureRuleModel("<<<NFC>>>", "")
  )
}
