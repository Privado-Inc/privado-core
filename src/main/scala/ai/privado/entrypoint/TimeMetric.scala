package ai.privado.entrypoint

import java.util.{Calendar, Date}

object TimeMetric {
  val cal           = Calendar.getInstance()
  val start         = cal.getTime
  var lastTime      = start
  var newTime       = start
  var stageLastTime = start

  def getNewTime(): Date = {
    newTime = Calendar.getInstance().getTime
    newTime
  }

  def getNewTimeAndSetItToStageLast(): Date = {
    stageLastTime = getNewTime()
    stageLastTime
  }

  def setNewTimeToStageLastAndGetTimeDiff(): String = {
    val diff = newTime.getTime - stageLastTime.getTime
    stageLastTime = newTime
    getDiffFormatted(diff)
  }

  def setNewTimeToLastAndGetTimeDiff(): String = {
    val diff = newTime.getTime - lastTime.getTime
    lastTime = newTime
    getDiffFormatted(diff)
  }

  private def getDiffFormatted(diff: Long): String = {
    val seconds = diff / 1000
    val ms      = diff                  % 1000
    val s       = seconds               % 60
    val m       = (seconds / 60)        % 60
    val h       = (seconds / (60 * 60)) % 24
    String.format("%d ms - %02dh:%02dm:%02ds:%02dms", diff, h, m, s, ms)
  }

  def getTheTotalTime(): String = {
    val diff = newTime.getTime - start.getTime
    getDiffFormatted(diff)
  }

}
