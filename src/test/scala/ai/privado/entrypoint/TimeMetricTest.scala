package ai.privado.entrypoint

import ai.privado.entrypoint.TimeMetric.cal
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.util.Calendar

class TimeMetricTest extends AnyWordSpec with Matchers with BeforeAndAfterAll {

  // TODO Need to change TimeMetric to being an instance, instead of global object
  "Millisecond String format " ignore {
    "Millisecond" in {
      TimeMetric.cal.add(Calendar.MILLISECOND, 10)
      TimeMetric.newTime = TimeMetric.cal.getTime
      val str = TimeMetric.setNewTimeToLastAndGetTimeDiff()
      str shouldBe "10 ms - 00h:00m:00s:10ms"
    }
    "Hours, Min, Second and Milliseconds" in {
      TimeMetric.cal.add(Calendar.MILLISECOND, 10)
      TimeMetric.cal.add(Calendar.SECOND, 11)
      TimeMetric.cal.add(Calendar.MINUTE, 12)
      TimeMetric.cal.add(Calendar.HOUR, 13)
      TimeMetric.newTime = TimeMetric.cal.getTime
      val str = TimeMetric.setNewTimeToLastAndGetTimeDiff()
      str shouldBe "47531010 ms - 13h:12m:11s:10ms"
    }
  }
}
