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
  val poses = List("unkown", "at_garage", "at_wp1", "at_bin1", "at_bin2")
  val initialPose = poses.head
  val state = List("idle", "charging", "planning", "executing")
  val initialState = state.head
}


class Robot(override val system: ActorSystem) extends ROSResource {
  val actPos = i("actPos", RobotData.initialPose, RobotData.poses.map(SPValue(_)))
  val subMapping = stringToIDMapper(Map("act_pos" -> actPos))
  val subFlow = subMapping
  subscribe("/unicorn_roscontrol/robot_uni_to_sp", "unicorn_ros2_messages/something", subFlow)

  val refPos = o("refPos", RobotData.initialPose, RobotData.poses.map(SPValue(_)))
  val pubMapping = IDToStringMapper(Map(refPos -> "ref_pos"))
  val pubFlow = pubMapping
  publish("/unification_roscontrol/ur_moveit_sp_to_unidriver", "unification_ros2_messages/MoveItSPToUni", Some(1000.millis), pubFlow)

  v("state", RobotData.initialState, RobotData.state.map(SPValue(_)))

  a("moveToPos")(
    c("pre", "true"),
    c("started", "actPos != refPos"),
    c("post", "actPos == refPos"),
    c("reset", "true"))
}

class MondayDemo(override val system: ActorSystem) extends MiniModel {
  use("robot1", new Robot(system))
  use("robot2", new Robot(system))

  v("bin1", "empty", List("empty", "full"))
  v("bin2", "empty", List("empty", "full"))

  val robot1GotoBin1 = o("robot1.gotoBin1", "robot1.moveToPos", "robot1")(
    c("pre", s"bin1 == 'full' && robot1.state == 'idle'", "robot1.refPos := 'at_bin1'"),
    c("post", "true"),
    c("reset", "true")
  )

  addBookings()
  synthesize()

}
