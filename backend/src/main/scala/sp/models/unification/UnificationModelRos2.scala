package sp.models.unification

import scala.concurrent.duration._

import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }

import akka.actor._

import sp.domain.Logic._
import sp.domain._

import sp.modelSupport._
import sp.modelSupport.ros2._


class HECU(override val system: ActorSystem) extends ROSResource {
  val lf_tool_home = i("lf_tool_home", false)
  val filter_tool_home = i("filter_tool_home", false)


  val subMapping = stringToIDMapper(Map(
    "lf_tool_home" -> lf_tool_home,
    "filter_tool_home" -> filter_tool_home,
  ))

  val subFlow = subMapping
  subscribe("/unification_roscontrol/hecu_uni_to_sp", "unification_ros2_messages/HecuUniToSP", subFlow)
}

class RECU(override val system: ActorSystem) extends ROSResource {
  val lock_rsp = o("lock_rsp", false)
  val unlock_rsp = o("unlock_rsp", false)

  val pubMapping = IDToStringMapper(Map(
    lock_rsp -> "lock_rsp",
    unlock_rsp -> "unlock_rsp"
  ))

  publish("/unification_roscontrol/recu_uni_to_sp", "unification_ros2_messages/RecuUniToSP", Some(100 millis), pubMapping)

  a("lock")(
    c("pre", "true", "lock_rsp := true"),
    c("post", "true"),
    c("reset", "true", "lock_rsp := false"))

  a("unlock")(
    c("pre", "true", "unlock_rsp := true"),
    c("post", "true"),
    c("reset", "true", "unlock_rsp := false"))

}

class UnificationModel(override val system: ActorSystem) extends MiniModel {
  use("hecu", new HECU(system))
  use("recu", new RECU(system))




  val lock = o("lock", "recu.lock", "recu")(
    c("pre", "hecu.lf_tool_home"),
    c("post", "true"),
    c("reset", "true")
  )

  val ulock = o("unlock", "recu.unlock", "recu")(
    c("pre", "!hecu.lf_tool_home"),
    c("post", "true"),
    c("reset", "true")
  )


  // resource bookings
  addBookings()

  // synthesis
  // synthesize()

  exportNuXmv("unificationRos2.smv")
}
