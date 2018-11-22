package sp.unicorn.mondaydemo

import scala.concurrent.duration._

import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }

import akka.actor._

import sp.domain.Logic._
import sp.domain._

import sp.modelSupport._
import sp.modelSupport.ros2._

object RobotData {
  val poses = List("unkown", "UWS", "G1", "G2", "C1", "C2", "C3", "C4", "C5", "GDP1F1", "GDP1F2", "GDP2F1", "GDP2F2")
  val initialPose = "G1" // poses.head
}


class Robot(name: String, override val system: ActorSystem) extends ROSResource {
  val actPos = i("actPos", RobotData.initialPose, RobotData.poses.map(SPValue(_)))

  val subMapping = stringToIDMapper(Map("act_pos" -> actPos))
  val subFlow = subMapping
  subscribe(s"/unicorn_roscontrol/hrp$name/hrp_uni_to_sp", "unicorn_ros2_messages/HrpUniToSP", subFlow)

  val refPos = o("refPos", RobotData.initialPose, RobotData.poses.map(SPValue(_)))
  val pubMapping = IDToStringMapper(Map(refPos -> "ref_pos"))
  val pubFlow = pubMapping
  publish(s"/unicorn_roscontrol/robot$name/hrp_sp_to_uni", "unicorn_ros2_messages/HrpSPToUni", Some(1000.millis), pubFlow)

  v("hasBin", false)

  a("planAndMove")(
    c("pre", "true"),
    c("post", "actPos == refPos"),
    c("reset", "true"))

}

class MondayDemo(override val system: ActorSystem) extends MiniModel {
  use("robot1", new Robot("1", system))
  use("robot2", new Robot("2", system))

  val bins = List("GDP1F1", "GDP1F2", "GDP2F1", "GDP2F2")

  bins.foreach { b =>
    i(b, "empty", List("empty", "full", "byRobot"))
    o(s"fill$b")(
      c("pre", s"$b == 'empty'"),
      c("post", s"$b == 'full'"),
      c("reset", "true")
    )
  }

  val robots = List("robot1", "robot2")

  // goto all poses except "unknown"
  val gotos: List[(String, String)] = List(
    // out of garage
    "G1" -> "C1",
    "G2" -> "C1",
    // via points going to bins
    "C1" -> "C2",
    "C2" -> "GDP1F1",
    "C2" -> "GDP1F2",
    "C2" -> "GDP2F1",
    "C2" -> "GDP2F2",
    // via points returning to the uws
    "GDP1F1" -> "C2",
    "GDP1F2" -> "C2",
    "GDP2F1" -> "C2",
    "GDP2F2" -> "C2",
    "C2" -> "C5",
    "C5" -> "UWS",
    // back to garage
    "UWS" -> "G1",
    "UWS" -> "G2",
  )

  def gotoName(a: String, b: String): String = s"${a}to${b}"

  for {
    r <- robots
    (a,b) <- gotos
  } yield {
    val movement = gotoName(a,b)
    o(s"$r.go$movement", s"$r.planAndMove", List(r,a,b))(
      c("pre", s"$r.actPos == '$a'", s"$r.refPos := '$b'"),
      c("post", "true"),
      c("reset", "true")
    )
  }

  for {
    r <- robots
    b <- bins
  } yield {
    o(s"$r.pick$b")(
      c("pre", s"$r.actPos == '$b' && $b == 'full'", s"$b := 'byRobot'"),
      c("post", "true"),
      c("reset", "true")
    )
  }

  addBookings()
  synthesize("unicorn", false)

}
