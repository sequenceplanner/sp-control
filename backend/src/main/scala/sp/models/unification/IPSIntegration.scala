package sp.models.unification.ipsintegration

import scala.concurrent.duration._

import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }

import akka.actor._

import sp.domain.Logic._
import sp.domain._

import sp.modelSupport._
import sp.modelSupport.ros2._

object UR {
  val initialState = "unknown"

  val poses = List("unknown",
    "PRE_ATTACH_OF", "ATTACH_OF",

    "PRE_OF_1_UNTIGHTENED", "PRE_OF_1_TIGHTENED",
    "OF_1_UNTIGHTENED", "OF_1_TIGHTENED",

    "PRE_OF_2_UNTIGHTENED", "PRE_OF_2_TIGHTENED",
    "OF_2_UNTIGHTENED", "OF_2_TIGHTENED",
  )
}


class UR(override val system: ActorSystem) extends ROSResource {

  // {"actual_pose":"","moving":false,"ricochet":{"got_goal_tolerance":"","got_acc_scaling":"","got_speed_scaling":"","got_pose_name":"","got_pose_type":"","got_robot_name":"","got_robot_type":"","got_action":""},"info":{"error_list":[],"got_reset":false,"t_plus":"","fresh_msg":false,"robot_name":""}}

  val actPos = i("actPos", UR.initialState, UR.poses.map(SPValue(_)))

  val subMapping = stringToIDMapper(Map("actual_pose" -> actPos))

  val subFlow = subMapping
  subscribe("/unification_roscontrol/ur_TARS_pose_unidriver_uni_to_sp", "unification_ros2_messages/URPoseUniToSP", subFlow)

  val refPos = o("refPos", UR.initialState, UR.poses.map(SPValue(_)))
  val pubMapping = IDToStringMapper(Map(refPos -> "pose_name"))


  // {"goal_tolerance":0,"acc_scaling":0,"speed_scaling":0,"pose_name":"","pose_type":"","robot_name":"","robot_type":"","action":""}

  // action is either PLANNED, MOVEJ, MOVEL


  // this source can be anything! for instance configured in the frontend
  val initialProps = Map(
    "goal_tolerance" -> SPValue(0.001),
    "acc_scaling" -> SPValue(0.1),
    "speed_scaling" -> SPValue(0.1),
    "pose_type" -> SPValue("TCP"),
    "robot_name" -> SPValue("TARS"),
    "robot_type" -> SPValue("ur10")
  )

  // POSE type is JOINT or TCP

  // "pose_name"  is what we set
  val staticProps = Source.repeat(initialProps)
  // flow to set the correct "pose_type"
  val poseType = Flow[Map[String, SPValue]].map { case out =>
    val extraField = out.get("pose_name").map{refVal =>
      if(refVal.asOpt[String].map(str => str == "OF_1_TIGHTENED").getOrElse(false))
        "pose_type" -> SPValue("JOINT")
      else
        "pose_type" -> SPValue("TCP")
    }
    out ++ extraField.toList
  }

  // val poseType = Flow[Map[String, SPValue]].map { case out => out }

  val actionType = Flow[Map[String, SPValue]].map { case out =>
    val extraField = out.get("pose_name").map{refVal =>
      if(refVal.asOpt[String].map(str => str.startsWith("PRE_")).getOrElse(false))
        "action" -> SPValue("MOVEL")
      else
        "action" -> SPValue("PLANNED")
    }
    out ++ extraField.toList
  }

  val pubFlow = pubMapping.via(poseType).via(actionType).zipWith(staticProps)(_++_)

  publish("/unification_roscontrol/ur_pose_unidriver_sp_to_uni", "unification_ros2_messages/URPoseSPToUni", Some(1000.millis), pubFlow)

  a("moveToPos")(
    c("pre", "true"),
    c("started", "actPos != refPos"),
    c("post", "actPos == refPos"),
    c("reset", "true"))
}


class IPSIntegrationModel(override val system: ActorSystem) extends MiniModel {
  use("ur", new UR(system))

  v("robot_has_tool", false)

  val urGotoPreAttach = o("ur.gotoPreAttach", "ur.moveToPos", "ur")(
    c("pre", s"!robot_has_tool || ur.actPos == 'ATTACH_OF'", "ur.refPos := 'PRE_ATTACH_OF'"),
    c("post", "true"),
    c("reset", "true")
  )

  val urGotoAttach = o("ur.gotoAttach", "ur.moveToPos", "ur")(
    c("pre", s"!robot_has_tool && ur.actPos == 'PRE_ATTACH_OF'", "ur.refPos := 'ATTACH_OF'"),
    c("post", "true", "robot_has_tool := true"),
    c("reset", "true")
  )

  val urGotoPreOf1UT = o("ur.gotoPreOf1UT", "ur.moveToPos", "ur")(
    c("pre", s"robot_has_tool && ur.actPos == 'PRE_ATTACH_OF'", "ur.refPos := 'PRE_OF_1_UNTIGHTENED'"),
    c("post", "true"),
    c("reset", "true")
  )

  val urGotoOf1UT = o("ur.gotoPreOf1UT", "ur.moveToPos", "ur")(
    c("pre", s"robot_has_tool && ur.actPos == 'PRE_OF_1_UNTIGHTENED'", "ur.refPos := 'OF_1_UNTIGHTENED'"),
    c("post", "true"),
    c("reset", "true")
  )

  sop("UR", List(Sequence(List(SOP(urGotoPreAttach), SOP(urGotoAttach)))))

  // resource bookings
  addBookings()
}
