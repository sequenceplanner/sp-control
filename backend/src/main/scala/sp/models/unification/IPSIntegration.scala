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
  val initialState = "UNKNOWN"

  val poses = List("UNKNOWN",
    "PRE_ATTACH_OF", "ATTACH_OF",

    "PRE_OF_1_UNTIGHTENED",
    "OF_1_TIGHTENED",

    "PRE_OF_2_UNTIGHTENED",
    "OF_2_TIGHTENED",
  )

  def poseMapping(source: String, target: String) = (source, target) match {
    case ("ATTACH_OF", "PRE_ATTACH_OF") => ("JOINT", "MOVEJ") // ("TCP", "MOVEL")
    case ("PRE_ATTACH_OF", "ATTACH_OF") => ("JOINT", "MOVEJ") // ("TCP", "MOVEL")
    case (_, "PRE_ATTACH_OF") => ("JOINT", "PLANNED")

    case ("PRE_OF_1_UNTIGHTENED", "OF_1_TIGHTENED") => ("JOINT", "MOVEJ") //("TCP", "MOVEL")
    case (_, "OF_1_TIGHTENED") => ("JOINT", "MOVEJ")

    case ("OF_1_TIGHTENED", "PRE_OF_1_UNTIGHTENED") => ("JOINT", "MOVEJ")
    case (_, "PRE_OF_1_UNTIGHTENED") => ("JOINT", "PLANNED")

    case ("PRE_OF_2_UNTIGHTENED", "OF_2_TIGHTENED") => ("JOINT", "MOVEJ") // ("TCP", "MOVEL")
    case (_, "OF_2_TIGHTENED") => ("JOINT", "MOVEJ")

    case ("OF_2_TIGHTENED", "PRE_OF_2_UNTIGHTENED") => ("JOINT", "MOVEJ")
    case (_, "PRE_OF_2_UNTIGHTENED") => ("JOINT", "PLANNED")

      // hack for old csv file
    case (_, target) if target.contains("JOINTPose") => ("JOINT", "PLANNED")
    case (_, target) if target.contains("TCPPose") => ("TCP", "PLANNED")

    case _ => // println(s"DEFAULT CASE for $source -> $target");
      ("JOINT", "PLANNED")
  }

  val needsOf = List("PRE_OF_1_UNTIGHTENED", "PRE_OF_1_TIGHTENED",
    "OF_1_UNTIGHTENED", "OF_1_TIGHTENED",

    "PRE_OF_2_UNTIGHTENED", "PRE_OF_2_TIGHTENED",
    "OF_2_UNTIGHTENED", "OF_2_TIGHTENED",
  )
  val canOnlyGoFrom = Map(
    "ATTACH_OF" -> "PRE_ATTACH_OF",
    "PRE_OF_1_UNTIGHTENED" -> "PRE_ATTACH_OF",
    "PRE_OF_2_UNTIGHTENED" -> "PRE_ATTACH_OF",
    "OF_1_TIGHTENED" -> "PRE_OF_1_UNTIGHTENED",
    "OF_2_TIGHTENED" -> "PRE_OF_2_UNTIGHTENED",
  )

}


class UR(override val system: ActorSystem) extends ROSResource {
  val actPos = i("actPos", UR.initialState, UR.poses.map(SPValue(_)))
  val prevPos = i("prevPos", UR.initialState, UR.poses.map(SPValue(_))) /// internal mirror of the last seen actual position

  val subMapping = stringToIDMapper(Map("actual_pose" -> actPos,
    "previous_pose" -> prevPos))

  val prevMapping = Flow[Map[String, SPValue]].map{ state =>
    println(state)
    state ++ state.get("actual_pose").toList.filter(_!=SPValue("UNKNOWN")).map(spval => {println("AAA: " + spval); "previous_pose" -> spval})
  }

  val subFlow = prevMapping.via(subMapping)
  subscribe("/unification_roscontrol/ur_TARS_pose_unidriver_uni_to_sp", "unification_ros2_messages/URPoseUniToSP", subFlow)

  val refPos = o("refPos", UR.initialState, UR.poses.map(SPValue(_)))
  val pubMapping = IDToStringMapper(Map(refPos -> "pose_name"))

  val attached = o("ofAttached", false)
  val attachMapping = Flow[Map[ID, SPValue]].map { state =>
    state.get(attached).map{value =>
      if(value == SPValue(true)) Map("object_action" -> SPValue("ATTACH"), "object_name" -> SPValue("OFTOOL"))
      else Map("object_action" -> SPValue("DETACH"), "object_name" -> SPValue("OFTOOL"))
    }.getOrElse(Map[String, SPValue]())
  }

  // this source can be anything! for instance configured in the frontend
  val initialProps = Map(
    "goal_tolerance" -> SPValue(0.001),
    "acc_scaling" -> SPValue(0.1),
    "speed_scaling" -> SPValue(0.1),
    "robot_name" -> SPValue("TARS"),
    "robot_type" -> SPValue("UR10")
  )
  val staticProps = Source.repeat(initialProps)

  val poseMapping = Flow[Map[ID, SPValue]].map { state =>
    val extra = (for {
      refPos <- state.get(refPos)
      actPos <- state.get(prevPos)
    } yield {
      val (poseType, actionType) = UR.poseMapping(actPos.as[String], refPos.as[String])
      Map("pose_name" -> refPos, "pose_type" -> SPValue(poseType), "action" -> SPValue(actionType))
    }).getOrElse(Map[String, SPValue]())
    extra
  }

  val pubFlow = poseMapping.zipWith(staticProps)(_++_)

  publish("/unification_roscontrol/ur_pose_unidriver_sp_to_uni", "unification_ros2_messages/URPoseSPToUni", Some(1000.millis), pubFlow)
  publish("/unification_roscontrol/scene_updater_sp_to_uni", "unification_ros2_messages/SceneUpdaterSPToUni", Some(1000.millis), attachMapping)

  a("moveToPos", List(refPos))(
    c("pre", "true"),
    c("isExecuting", "actPos != refPos"),
    c("isFinished", "actPos == refPos"),
    c("reset", "true"))
}

class AECU(override val system: ActorSystem) extends ROSResource {
  val tool_is_idle = i("tool_is_idle", false)
  val tool_is_running_forward = i("tool_is_running_forward", false)
  // bool tool_is_running_reverse
  // bool tool_is_in_alarm
  // bool positioned_at_home_station
  // bool operating_position
  // bool pre_home_position
  // bool unclear_position
  val ptr = i("programmed_torque_reached", false)
  val home = i("positioned_at_home_station", false)

  val subMapping = createInputMappingFromMessageType("unification_ros2_messages/AecuUniToSP")
  subscribe("/unification_roscontrol/aecu_uni_to_sp", "unification_ros2_messages/AecuUniToSP", subMapping)

  val set_tool_idle = o("set_tool_idle", true)
  val run_tool_forward = o("run_tool_forward", false)
  // val run_tool_in_reverse
  // val inhibit_all_run_also_manual
  // val activate_unload
  val activate_unload = o("activate_unload", false)
  // val activate_lift
  val activate_lift = o("activate_lift", false)

  val pubMapping = createOutputMappingFromMessageType("unification_ros2_messages/AecuSPToUni")

  publish("/unification_roscontrol/aecu_sp_to_uni", "unification_ros2_messages/AecuSPToUni", Some(1000.millis), pubMapping)

  a("startToolForward", List())(
    c("pre", "tool_is_idle", "run_tool_forward:=true", "set_tool_idle := false"),
    c("isExecuting", "!tool_is_running_forward"),
    c("isFinished", "tool_is_running_forward"),
  )
}

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
  // bool robot_not_connected_to_tool
  // bool robot_connected_to_lf_tool
  // bool robot_connected_to_atlas_tool
  // bool robot_connected_to_filter_tool
  // bool undefined_connection_detected
  // bool robot_tool_connection_failure
  // bool ladder_frame_not_connected
  // bool ladder_frame_connected
  // bool ladder_frame_connection_failure
  // bool pressure_ok

  val pressure = i("pressure_ok", false)

  val subMapping = stringToIDMapper(Map("pressure_ok" -> pressure))
  subscribe("/unification_roscontrol/recu_uni_to_sp", "unification_ros2_messages/RecuUniToSP", subMapping)

  val lock_rsp = o("lock_rsp", false)
  val unlock_rsp = o("unlock_rsp", false)

  val pubMapping = IDToStringMapper(Map(
    lock_rsp -> "lock_rsp",
    unlock_rsp -> "unlock_rsp"
  ))

  publish("/unification_roscontrol/recu_sp_to_uni", "unification_ros2_messages/RecuSPToUni", Some(100 millis), pubMapping)

  a("lock")(
    c("pre", "true", "lock_rsp := true"),
    c("reset", "true")
  )

  a("unlock")(
    c("pre", "true", "unlock_rsp := true"),
    c("reset", "true")
  )

}

class IPSIntegrationModel(override val system: ActorSystem) extends MiniModel {
  use("ur", new UR(system))
  use("aecu", new AECU(system))
  use("recu", new RECU(system))
  use("hecu", new HECU(system))


  val startMotor = o(s"aecu.startTool", "aecu.startToolForward", "aecu")()

  v("bolt1Tightened", false)

  o("watchForBolt1Tightened")(
    c("isExecuting", "!aecu.programmed_torque_reached && ur.actPos == 'OF_1_TIGHTENED'"),
    c("isFinished", "aecu.programmed_torque_reached && ur.actPos == 'OF_1_TIGHTENED'", "bolt1Tightened := true")
  )

  v("of1", false)

  val gotoPositions = UR.poses.filter(_!="UNKNOWN").map { p =>
    val of = UR.needsOf.contains(p)
    val ofc = if(of) c("pre", s"ur.ofAttached") else c("pre", "true")
    val of1 = if(p == "OF_1_TIGHTENED") c("isFinished", "true", "of1 := true") else c("isFinished", "true")

    val source = UR.canOnlyGoFrom.get(p).map(source => c("pre", s"ur.actPos == '$source'")).getOrElse(c("pre", "true"))

    val op = o(s"ur.goto$p", "ur.moveToPos", "ur")(
      c("pre", s"ur.actPos != '$p' && ur.refPos != '$p'", s"ur.refPos := '$p'"),
      ofc,
      source,
      c("isFinished", "true"),
      of1,
      c("reset", "true")
    )
    (p, op)
  }.toMap

  val attach = o("ur.attach")(
    c("pre", s"!ur.ofAttached && ur.actPos == 'ATTACH_OF' && ur.refPos == 'ATTACH_OF'", "ur.ofAttached := true"),
//    c("isExecuting", "false"),
//    c("isFinished", "ur.ofAttached")
  )

  val detach = o("ur.detach")(
    c("pre", s"ur.ofAttached && ur.actPos == 'ATTACH_OF' && ur.refPos == 'ATTACH_OF'", "ur.ofAttached := false"),
//    c("isExecuting", "false"),
//    c("isFinished", "!ur.ofAttached")
  )

  val humanState = v("human", "idle", List("idle", "tightening", "reset"))
  val humanDone = i("humanDone", false)

  val humanTighten = o("human.tighten")(
    c("pre", s"human == 'idle' && ur.actPos != 'OF_1_TIGHTENED' && ur.refPos != 'OF_1_TIGHTENED'", "human := 'tightening'"),
    c("isExecuting", "human == 'tightening' && !humanDone"),
    c("isFinished", "human == 'tightening' && humanDone", "of1 := true", "human := 'reset'")
  )

  val humanReset = o("human.reset")(
    c("pre", s"human == reset", "human := idle")
  )

  val tighten1 = SPAttributes(
    "name" -> "tighten",
    "pre" -> "ur.actPos == _'PRE_ATTACH_OF'",
    "goal" -> "ur.actPos == 'OF_1_TIGHTENED'",
  )

  // just build a simple sop to visualize the operation states
  // make a grid with four columns to utilize the space we have in the widget
  val grid = List(0,1,2,3).map(n=>operations.sliding(4,4).flatMap(_.lift(n)).toList)
  sop("sop", List(Parallel(grid.map(s=>Sequence(s.map(o=>SOP(o)))))))

  // resource bookings
  addBookings()

  exportNuXmv("ipsintegration.smv")
}
