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

class UnificationModel(override val system: ActorSystem) extends MiniModel {
  use("hecu", new HECU(system))

  // resource bookings
  addBookings()


  // synthesis
  // synthesize()
}
