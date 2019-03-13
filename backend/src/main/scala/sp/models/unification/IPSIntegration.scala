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

  val poses2 = List("UNKNOWN",
    "PRE_ATTACH_OF", "ATTACH_OF",

    "PRE_OF_1_UNTIGHTENED",
    "OF_1_TIGHTENED",

    "PRE_OF_2_UNTIGHTENED",
    "OF_2_TIGHTENED",
  )

  val poses = List("UNKNOWN",
    "ATTACH_OF",
    "PRE_ATTACH_OF",
    "PRE_OF_1_UNTIGHTENED",
    "OF_1_TIGHTENED",
    "PRE_OF_2_UNTIGHTENED",
    "OF_2_TIGHTENED",
///////// old Joint poses
    // "HomeJOINTPose",
    // "PreHomeJOINTPose",
    // "PreAttachAtlasFarJOINTPose",
    "PreAttachLFToolFarJOINTPose",
    "PreAttachOFToolFarJOINTPose",
    // "AboveEngineJOINTPose",
    // "PreFindEngineJOINTPose",
    // "FindEngineRightUpJOINTPose",
    // "FindEngineLeftUpJOINTPose",
    // "FindEngineMidUpJOINTPose",
    "LFOperationMidpoint1JOINTPose",
    "LFOperationMidpoint2JOINTPose",
    "LFOperationMidpoint3JOINTPose",
    "LFOperationMidpoint4JOINTPose",
    "LFOperationMidpoint5JOINTPose",
    "AfterLFOperationJOINTPose",
    // "OFMidpoint1JOINTPose",
    // "OFMidpoint2JOINTPose",
    // "AboveUntightenedOF1JOINTPose",
    // "AtUntightenedOF1JOINTPose",
    // "AtTightenedOF1JOINTPose",
    // "AboveTightenedOF1JOINTPose",
    // "AboveUntightenedOF2JOINTPose",
    // "AtUntightenedOF2JOINTPose",
    // "AtTightenedOF2JOINTPose",
    // "AboveTightenedOF2JOINTPose",
///////// old TCP poses
    // "FindEngineRightDownTCPPose",
    // "FindEngineRightCollideTCPPose",
    // "FindEngineLeftDownTCPPose",
    // "FindEngineLeftCollideTCPPose",
    // "FindEngineMidDownTCPPose",
    // "FindEngineMidCollideTCPPose",
    // "PreAttachAtlasCloseTCPPose",
    // "AttachAtlasTCPPose",
    // "AAPRAtlasTCPPose",
    "PreAttachLFToolCloseTCPPose",
    "AttachLFToolTCPPose",
    "AAPRLFToolTCPPose",
    "PreAttachOFToolCloseTCPPose",
    "AttachOFToolTCPPose",
    "AAPROFTool1TCPPose",
    "AAPROFTool2TCPPose",
    // "AboveEngineTCPPose",
    // "FarAboveBoltPair1TCPPose",
    // "CloseAboveBoltPair1TCPPose",
    // "AtBoltPair1TCPPose",
    // "FarAboveBoltPair2TCPPose",
    // "CloseAboveBoltPair2TCPPose",
    // "AtBoltPair2TCPPose",
    // "FarAboveBoltPair3TCPPose",
    // "CloseAboveBoltPair3TCPPose",
    // "AtBoltPair3TCPPose",
    // "FarAboveBoltPair4TCPPose",
    // "CloseAboveBoltPair4TCPPose",
    // "AtBoltPair4TCPPose",
    // "FarAboveBoltPair5TCPPose",
    // "CloseAboveBoltPair5TCPPose",
    // "AtBoltPair5TCPPose",
    // "FarAboveBoltPair6TCPPose",
    // "CloseAboveBoltPair6TCPPose",
    // "AtBoltPair6TCPPose",
    // "FarAboveBoltPair7TCPPose",
    // "CloseAboveBoltPair7TCPPose",
    // "AtBoltPair7TCPPose",
    // "FarAboveBoltPair8TCPPose",
    // "CloseAboveBoltPair8TCPPose",
    // "AtBoltPair8TCPPose",
    // "FarAboveBoltPair9TCPPose",
    // "CloseAboveBoltPair9TCPPose",
    // "AtBoltPair9TCPPose",
    // "FarAboveBoltPair10TCPPose",
    // "CloseAboveBoltPair10TCPPose",
    // "AtBoltPair10TCPPose",
    // "FarAboveBoltPair11TCPPose",
    // "CloseAboveBoltPair11TCPPose",
    // "AtBoltPair11TCPPose",
    // "FarAboveBoltPair12TCPPose",
    // "CloseAboveBoltPair12TCPPose",
    // "AtBoltPair12TCPPose",
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
    case (_, target) if target.contains("TCPPose") => ("TCP", "MOVEL")

    case _ => // println(s"DEFAULT CASE for $source -> $target");
      ("JOINT", "PLANNED")
  }

  val needsOf = List("PRE_OF_1_UNTIGHTENED", "PRE_OF_1_TIGHTENED",
    "OF_1_UNTIGHTENED", "OF_1_TIGHTENED",

    "PRE_OF_2_UNTIGHTENED", "PRE_OF_2_TIGHTENED",
    "OF_2_UNTIGHTENED", "OF_2_TIGHTENED",

    "AAPROFTool1TCPPose", // getting and leaving the tool via these
    "AAPROFTool2TCPPose",

  )
  val canOnlyGoFrom = Map(
    "ATTACH_OF" -> "PRE_ATTACH_OF",
    "PRE_OF_1_UNTIGHTENED" -> "PRE_ATTACH_OF",
    "PRE_OF_2_UNTIGHTENED" -> "PRE_ATTACH_OF",
    "OF_1_TIGHTENED" -> "PRE_OF_1_UNTIGHTENED",
    "OF_2_TIGHTENED" -> "PRE_OF_2_UNTIGHTENED",
  )

  val OFToolPoses = List("PreAttachOFToolCloseTCPPose", "AttachOFToolTCPPose", "AAPROFTool1TCPose", "AAPROFTool2TCPose")
  val moveMapOFToolNoTool = Map(
    "PreAttachOFToolFarJOINTPose" -> List("AAPROFTool2TCPPose", "PRE_ATTACH_OF", "AttachOFToolTCPPose"),
    "PreAttachOFToolCloseTCPPose" -> List("PreAttachOFToolFarJOINTPose"),
    "AttachOFToolTCPPose" -> List("PreAttachOFToolCloseTCPPose"), // , "AAPROFTool1TCPPose"),
    "AAPROFTool1TCPPose" -> List("AAPROFTool2TCPPose"),
    "AAPROFTool2TCPPose" -> List("AAPROFTool1TCPPose", "PreAttachOFToolFarJOINTPose"),
  )

  val moveMapOFToolWithTool = Map(
    "PreAttachOFToolFarJOINTPose" -> List("AAPROFTool2TCPPose", "PRE_ATTACH_OF"),
    "PreAttachOFToolCloseTCPPose" -> List(),
    "AttachOFToolTCPPose" -> List("AAPROFTool1TCPPose"),
    "AAPROFTool1TCPPose" -> List("AAPROFTool2TCPPose", "AttachOFToolTCPPose"),
    "AAPROFTool2TCPPose" -> List("AAPROFTool1TCPPose", "PreAttachOFToolFarJOINTPose"),
  )






  // allowed movements witout the tool
  // PreAttachLFToolFarJOINT from PRE_ATTACH_OF, PreAttachLFToolCloseTCP
  // PreAttachLFToolCloseTCP from PreAttachLFToolFarJOINT, AttachLFToolTCP,
  // AttachLFToolTCP from PreAttachLFToolCloseTCP,
  // AAPRLFToolTCP from List()
  val LFToolPoses = List("PreAttachLFToolFarJOINTPose", "PreAttachLFToolCloseTCPPose", "AttachLFToolTCPPose", "AAPRLFToolTCPPose")
  val moveMapLFToolNoTool = Map(
    "PreAttachLFToolFarJOINTPose" -> List("PRE_ATTACH_OF", "PreAttachLFToolCloseTCPPose"),
    "PreAttachLFToolCloseTCPPose" -> List("PreAttachLFToolFarJOINTPose", "AttachLFToolTCPPose"),
    "AttachLFToolTCPPose" -> List("PreAttachLFToolCloseTCPPose"),
    "AAPRLFToolTCPPose" -> List()
  )

  // allowed movements with the tool:
  // PreAttachLFToolFarJOINT from PRE_ATTACH_OF, AAPRLFToolTCP
  // PreAttachLFToolCloseTCP from List(),
  // AttachLFToolTCP from AAPRLFToolTCP,
  // AAPRLFToolTCP from PreAttachLFToolFarJOINT with gripper open
  val moveMapLFToolWithTool = Map(
    "PreAttachLFToolFarJOINTPose" -> List("PRE_ATTACH_OF", "AAPRLFToolTCPPose"),
    "PreAttachLFToolCloseTCPPose" -> List(),
    "AttachLFToolTCPPose" -> List("AAPRLFToolTCPPose"),
    "AAPRLFToolTCPPose" -> List("AttachLFToolTCPPose", "PreAttachLFToolFarJOINTPose") // with gripper open
  )



  val moveMapLFOp = Map(  // these all need the LF tool
    "LFOperationMidpoint1JOINTPose" -> List("PRE_ATTACH_OF", "LFOperationMidpoint2JOINTPose"),
    "LFOperationMidpoint2JOINTPose" -> List("LFOperationMidpoint1JOINTPose","LFOperationMidpoint3JOINTPose"),
    "LFOperationMidpoint3JOINTPose" -> List("LFOperationMidpoint2JOINTPose","LFOperationMidpoint4JOINTPose"),
    "LFOperationMidpoint4JOINTPose" -> List("LFOperationMidpoint3JOINTPose", "LFOperationMidpoint5JOINTPose"),
    "LFOperationMidpoint5JOINTPose" -> List("LFOperationMidpoint4JOINTPose")
  )

}


class UR(override val system: ActorSystem) extends ROSResource {
  val actPos = i("actPos", UR.initialState, UR.poses.map(SPValue(_)))
  val moving = i("moving", false)
  // val prevPos = i("prevPos", UR.initialState, UR.poses.map(SPValue(_))) /// internal mirror of the last seen actual position
  val prevPos = vm("prevPos", UR.initialState, UR.poses.map(SPValue(_)), Set(), SPAttributes("input" -> true, "notInModel" -> true))

  val subMapping = stringToIDMapper(Map("moving" -> moving, "actual_pose" -> actPos, "previous_pose" -> prevPos))

  val prevMapping = Flow[Map[String, SPValue]].map{ state =>
    state ++ state.get("actual_pose").toList.filter(_!=SPValue("UNKNOWN")).map(spval => "previous_pose" -> spval)
  }

  val subFlow = prevMapping.via(subMapping)
  subscribe("/unification_roscontrol/ur_TARS_pose_unidriver_uni_to_sp", "unification_ros2_messages/URPoseUniToSP", subFlow)

  val attachedInMoveit = i("isAttachedOFMoveit", false)
  val fromMoveitMapping = Flow[Map[String, SPValue]].map{ state =>
    val attached = state.get("attached_objects").flatMap(_.asOpt[List[String]]).map(_.contains("OFTOOL")).getOrElse(false)
    Map(attachedInMoveit -> SPValue(attached))
  }
  subscribe("/unification_roscontrol/scene_updater_uni_to_sp", "unification_ros2_messages/SceneUpdaterUniToSP", fromMoveitMapping)

  val refPos = o("refPos", UR.initialState, UR.poses.map(SPValue(_)))
  val pubMapping = IDToStringMapper(Map(refPos -> "pose_name"))

  val attachInMoveit = o("attachOFMoveit", false)
  val attachMapping = Flow[Map[ID, SPValue]].map { state =>
    state.get(attachInMoveit).map{value =>
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
    c("pre", "!moving"),
    c("startEffect", "true", "moving := true", "actPos := 'UNKNOWN'"),
    c("isExecuting", "moving || actPos != refPos"),
    c("executingEffect", "true", "moving := false", "actPos := refPos"),
    c("isFinished", "!moving && actPos == refPos"),
    c("reset", "true"))

  a("attachOFMoveit", List())(
    c("pre", "!isAttachedOFMoveit", "attachOFMoveit := true"),
    c("isExecuting", "attachOFMoveit && !isAttachedOFMoveit"),
    c("executingEffect", "true", "isAttachedOFMoveit := true"),
    c("isFinished", "attachOFMoveit && isAttachedOFMoveit"),
    c("reset", "true"))

  a("detachOFMoveit", List())(
    c("pre", "isAttachedOFMoveit", "attachOFMoveit := false"),
    c("isExecuting", "!attachOFMoveit && isAttachedOFMoveit"),
    c("executingEffect", "true", "isAttachedOFMoveit := false"),
    c("isFinished", "!attachOFMoveit && isAttachedOFMoveit == false"), // TODO: fix filter props...
    c("reset", "true"))
}

class AECU(override val system: ActorSystem) extends ROSResource {
  val tool_is_idle = i("tool_is_idle", true)
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
    c("executingEffect", "true", "tool_is_running_forward := true"),
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

  val notool = i("robot_not_connected_to_tool", true)
  val lftool = i("robot_connected_to_lf_tool", false)
  val oftool = i("robot_connected_to_filter_tool", false)
  val pressure = i("pressure_ok", false)

  val subMapping = createInputMappingFromMessageType("unification_ros2_messages/RecuUniToSP")
  subscribe("/unification_roscontrol/recu_uni_to_sp", "unification_ros2_messages/RecuUniToSP", subMapping)

  val lock_rsp = o("lock_rsp", false)
  val unlock_rsp = o("unlock_rsp", false)
  val open_gripper = o("open_gripper", false)
  val close_gripper = o("close_gripper", false)


  val pubMapping = createOutputMappingFromMessageType("unification_ros2_messages/RecuSPToUni")
  publish("/unification_roscontrol/recu_sp_to_uni", "unification_ros2_messages/RecuSPToUni", Some(100 millis), pubMapping)

  // there no feedback whether the tool is locked or not. same for the gripper
  // we can only know which specific tool has been attached

  a("lock")(
    c("pre", "true", "lock_rsp := true", "unlock_rsp := false"),
    c("isFinished", "lock_rsp == true && unlock_rsp == false"),
    c("reset", "true")
  )

  a("unlock")(
    c("pre", "true", "lock_rsp := false", "unlock_rsp := true"),
    c("isFinished", "lock_rsp == false && unlock_rsp == true"),
    c("reset", "true")
  )

  a("open")(
    c("pre", "robot_connected_to_lf_tool", "open_gripper := true", "close_gripper := false"),
    c("isExecuting", "false"),
    c("isFinished", "open_gripper == true && close_gripper == false"),
    c("reset", "true")
  )

  a("close")(
    c("pre", "robot_connected_to_lf_tool", "open_gripper := false", "close_gripper := true"),
    c("isExecuting", "false"),
    c("isFinished", "open_gripper == false && close_gripper == true"),  /// these should use the "auto-shadowed echo" variables...
    c("reset", "true")
  )

}

class IPSIntegrationModel(override val system: ActorSystem) extends MiniModel {
  use("ur", new UR(system))
  use("aecu", new AECU(system))
  use("recu", new RECU(system))
  use("hecu", new HECU(system))


  val startMotor = o(s"aecu.startToolForward", "aecu.startToolForward", "aecu")()

  // oil filters
  v("of1", false)
  v("of2", false)


  val gotoPositions = UR.poses.filter(_!="UNKNOWN").map { p =>

    // if we need the of tool, we need it both physically and in the planning interface
    val of = UR.needsOf.contains(p)
    val ofc = if(of) c("pre", s"ur.isAttachedOFMoveit && recu.robot_connected_to_filter_tool") else c("pre", "true")
    // PreAttachOFToolFarJOINTPose => cannot take this move from AttachOFToolTCPPose if we have the tool
    val of1 = if(p == "OF_1_TIGHTENED") c("isFinished", "true", "of1 := true") else c("isFinished", "true")

    // PRE_ATTACH_OF is always reachable. except from: PreAttachOFToolCloseTCPPose, AttachOFToolTCPPose, AAPROFTool1TCPose, AAPROFTool2TCPose,
    val preof =
      if(p == "PRE_ATTACH_OF") c("pre", s"ur.actPos != 'PreAttachOFToolCloseTCPPose' && ur.actPos != 'AttachOFToolTCPPose' && ur.actPos != 'AAPROFTool1TCPPose' && ur.actPos != 'AAPROFTool2TCPPose'")
      else c("pre", "true")

    val source = UR.canOnlyGoFrom.get(p).map(source => c("pre", s"ur.actPos == '$source'")).getOrElse(c("pre", "true"))

    val ofWithoutTool = UR.moveMapOFToolNoTool.get(p).map{source =>
      val pose = if(source.isEmpty) "false" else s"(${source.map(s=>s"ur.actPos == '$s'").mkString("||")})"
      s"(recu.robot_not_connected_to_tool && !recu.lock_rsp) && $pose"}.getOrElse("true")
    val ofWithTool = UR.moveMapOFToolWithTool.get(p).map{source =>
      val pose = if(source.isEmpty) "false" else s"(${source.map(s=>s"ur.actPos == '$s'").mkString("||")})"
      s"(recu.robot_connected_to_filter_tool && !recu.unlock_rsp) && $pose"}.getOrElse("true")
    val ofTool = c("pre", s"($ofWithoutTool || $ofWithTool)")

    val op = o(s"ur.goto$p", "ur.moveToPos", "ur")(
      c("pre", s"ur.actPos != '$p' && ur.refPos != '$p'", s"ur.refPos := '$p'"),
      ofc,
      source,
      ofTool,
      preof,
      c("isFinished", "true"),
      of1,
      c("reset", "true")
    )
    (p, op)
  }.toMap

  v("bolt1Tightened", false)
  o("watchForBolt1Tightened", attr = SPAttributes("ability" -> "yes"))(
    c("isExecuting", "aecu.startToolForward == 'finished' && (ur.gotoOF_1_TIGHTENED == 'executing' ||  ur.gotoOF_1_TIGHTENED == 'finished') && !aecu.programmed_torque_reached"),
    c("isFinished", "aecu.startToolForward == 'finished' &&(ur.gotoOF_1_TIGHTENED == 'executing' ||  ur.gotoOF_1_TIGHTENED == 'finished') && aecu.programmed_torque_reached", "bolt1Tightened := true")
  )


  //////// attach OF tool sequence
  // 1. PreAttachOFToolFarJOINT                     ---> PreAttachOFToolCloseTCP
  // 2. PreAttachOFToolCloseTCP                     ---> unlockRSP
  // 3. RSP is unlocked and PreAttachOFToolCloseTCP ---> AttachOFToolTCP
  // 4. AttachOFToolTCP                             ---> lockRSP --- we have the tool
  // 5. RSP is locked and AttachOFToolTCP           ---> AAPROFTool1TCP
  // 6. AAPROFTool1TCP and we have the tool         ---> AAPROFTool2TCP
  // 7. AAPROFTool2TCP                              ---> PreAttachOFToolFarJOINT
  // 8. PreAttachOFToolFarJOINT and have the tool   ---> done!

  //////// detach OF tool sequence
  // 1. PreAttachOFToolFarJOINT                     ---> AAPROFTool2TCP
  // 2. AAPROFTool2TCP                              ---> AAPROFTool1TCP
  // 3. AAPROFTool1TCP                              ---> AttachOFToolTCP
  // 4. AttachOFToolTCP                             ---> unlockrsp
  // 5. rsp unlocked and AttachOFToolTCP            ---> PreAttachOFToolFarJOINT
  // 6. PreAttachOFToolFarJOINT and no tool         ---> done!

  val unlockRspWhenConnectingOFTool = o("ofAttach.unlockRSP", "recu.unlock", "RSP")(
    c("pre", s"recu.robot_not_connected_to_tool && ur.actPos == 'PreAttachOFToolCloseTCPPose' && ur.refPos == 'PreAttachOFToolCloseTCPPose'"),
  )
  val lockRspWhenConnectingOFTool = o("ofAttach.lockRSP", "recu.lock", "ur")(
    c("pre", s"recu.robot_not_connected_to_tool && ur.actPos == 'AttachOFToolTCPPose' && ur.refPos == 'AttachOFToolTCPPose'"),
    c("isExecuting", s"recu.lock_rsp && recu.robot_not_connected_to_tool && ur.actPos == 'AttachOFToolTCPPose' && ur.refPos == 'AttachOFToolTCPPose'"),
    c("executingEffect", "true", "recu.robot_connected_to_filter_tool := true", "recu.robot_not_connected_to_tool := false"),
    c("isFinished", s"recu.robot_connected_to_filter_tool && ur.actPos == 'AttachOFToolTCPPose' && ur.refPos == 'AttachOFToolTCPPose'"),
  )
  val unlockRspWhenDisconnectingOFTool = o("ofDetach.unlockRSP", "recu.unlock", "ur")(
    c("pre", s"recu.robot_connected_to_filter_tool && !recu.robot_not_connected_to_tool && ur.actPos == 'AttachOFToolTCPPose' && ur.refPos == 'AttachOFToolTCPPose'"),
    c("isExecuting", s"recu.unlock_rsp && (recu.robot_connected_to_filter_tool || !recu.robot_not_connected_to_tool) && ur.actPos == 'AttachOFToolTCPPose' && ur.refPos == 'AttachOFToolTCPPose'"),
    c("executingEffect", "true", "recu.robot_connected_to_filter_tool := false", "recu.robot_not_connected_to_tool := true"),
    c("isFinished", s"recu.unlock_rsp && recu.robot_connected_to_filter_tool == false && recu.robot_not_connected_to_tool && ur.actPos == 'AttachOFToolTCPPose' && ur.refPos == 'AttachOFToolTCPPose'"),
  )

  val attachOFMoveit = o("ur.attachOFInMoveit", "ur.attachOFMoveit", "ur")(
    c("pre", s"ur.actPos == 'AttachOFToolTCPPose' && ur.refPos == 'AttachOFToolTCPPose'"),
  )

  val detachOFMoveit = o("ur.detachOFInMoveit", "ur.detachOFMoveit", "ur")(
    c("pre", s"ur.actPos == 'AttachOFToolTCPPose' && ur.refPos == 'AttachOFToolTCPPose'"),
  )



  //////// attach LF tool sequence
  // 1. PreAttachLFToolFarJOINT                       ---> PreAttachLFToolCloseTCP
  // 2. PreAttachLFToolCloseTCP                       ---> unlock rsp
  // 3. rsp unlocked and PreAttachLFToolCloseTCP      ---> AttachLFToolTCP
  // 4. AttachLFToolTCP                               ---> lock rsp
  // 5. rsp locked and AttachLFToolTCP                ---> open gripper
  // 6. gripper opened and AttachLFToolTCP            ---> AAPRLFToolTCP
  // 7. AAPRLFToolTCP                                 ---> PreAttachLFToolFarJOINT


  /////// detach LF tool sequence
  // 1. PreAttachLFToolFarJOINT                      ---> open gripper
  // 2. gripper opened and PreAttachLFToolFarJOINT   ---> AAPRLFToolTCP
  // 3. AAPRLFToolTCP and gripper opened             ---> close gripper
  // 4. AAPRLFToolTCP and gripper closed             ---> AttachLFToolTCP
  // 5. AttachLFToolTCP                              ---> unlock rsp
  // 6. AttachLFToolTCP and rsp unlocked             ---> PreAttachLFToolFarJOINT

  UR.LFToolPoses.foreach { p =>
    val withoutTool = UR.moveMapLFToolNoTool.get(p).map{source =>
      val pose = if(source.isEmpty) "false" else s"(${source.map(s=>s"ur.actPos == '$s'").mkString("||")})"
      s"(recu.robot_not_connected_to_tool && !recu.lock_rsp) && $pose"}.getOrElse("true")
    val withTool = UR.moveMapLFToolWithTool.get(p).map{source =>
      val pose = if(source.isEmpty) "false" else s"(${source.map(s=>s"ur.actPos == '$s'").mkString("||")})"
      s"(recu.robot_connected_to_lf_tool && recu.lock_rsp) && $pose"}.getOrElse("true")
    val lftool = c("pre", s"($withoutTool || $withTool)")
    o(s"ur.goto$p")(lftool)
  }

  // moving into position for LF magic operation
  UR.moveMapLFOp.foreach { case (target, sources) =>
    val sourcePose = if(sources.isEmpty) "false" else s"(${sources.map(s=>s"ur.actPos == '$s'").mkString("||")})"
    val hasTool = "recu.robot_connected_to_lf_tool"
    o(s"ur.goto$target")(c("pre", s"$sourcePose && $hasTool"))
  }

  // disable this move -- performed by the ur-script
  o(s"ur.gotoAfterLFOperationJOINTPose")(c("pre", "false"))

  // execute "lf magic" ur-script
  v("lf", "onMir", List("onMir", "onEngine"))
  v("doingLFMagic", false) // no feedback for now, fake it
  o("ur.lfmagic", bookings = Set("ur"), attr = SPAttributes("ability" -> "yes"))(
    c("pre", s"recu.robot_connected_to_lf_tool && ur.gotoLFOperationMidpoint5JOINTPose == 'finished'", "doingLFMagic := true", "ur.refPos := 'UNKNOWN'"),
    c("isExecuting", s"doingLFMagic"),
    c("executingEffect", "true", "ur.actPos := 'AfterLFOperationJOINTPose'"),
    c("isFinished", s"doingLFMagic && ur.actPos == 'AfterLFOperationJOINTPose'", "doingLFMagic := false", "lf := onEngine"),
  )


  // disable moving to home during tool change sequence
  o("ur.gotoPRE_ATTACH_OF")(
    c("pre", s"ur.actPos != 'AttachLFToolTCPPose' && ur.actPos != 'AAPRLFToolTCPPose' && ur.actPos != 'PreAttachLFToolCloseTCPPose'")
  )
  // gripper needs to be opened/closed during some moves
  o("recu.open", "recu.open", "ur")()
  o("recu.close", "recu.close", "ur")()

  o(s"ur.gotoAAPRLFToolTCPPose")(c("pre", "(recu.robot_connected_to_lf_tool && recu.open == 'finished') || recu.robot_not_connected_to_tool"))



  val unlockRspWhenNotHoldingATool = o("lfAttach.unlockRSP", "recu.unlock", "ur")(
    c("pre", s"recu.robot_not_connected_to_tool"),
  )
  val lockRspToConnectLFTool = o("lfAttach.lockRSP", "recu.lock", "ur")(
    c("pre", s"recu.robot_not_connected_to_tool && ur.gotoAttachLFToolTCPPose == 'finished'"),
    c("isExecuting", s"recu.lock_rsp && recu.robot_not_connected_to_tool && ur.gotoAttachLFToolTCPPose == 'finished'"),
    c("executingEffect", "true", "recu.robot_connected_to_lf_tool := true", "recu.robot_not_connected_to_tool := false"),
    c("isFinished", s"recu.robot_connected_to_lf_tool && ur.gotoAttachLFToolTCPPose == 'finished'"),
  )

  val humanState = v("human", "idle", List("idle", "executing", "finished"))
  val humanDone = i("humanDone", false)
  val humanAvailable = i("humanAvailable", true)

  val humanTighten = o("human.tighten", attr = SPAttributes("ability" -> "yes"))(
    c("pre", s"human == 'idle' && humanAvailable && ur.actPos != 'OF_1_TIGHTENED' && ur.refPos != 'OF_1_TIGHTENED'", "human := 'executing'"),
    c("isExecuting", "human == 'executing' && !humanDone"),
    c("executingEffect", "true", "humanDone := true"),
    c("isFinished", "human == 'executing' && humanDone", "of2 := true", "human := 'finished'")
  )

  val humanReset = o("human.reset", attr = SPAttributes("ability" -> "yes"))(
    c("pre", s"human == finished", "human := idle")
  )

  // val tightenState = v("tighten", "idle", List("idle", "active"))
  // val t = o("tighten", SPAttributes("notInModel" -> true, "hasGoal" -> "! F v_of1"))(
  //   c("pre", s"tighten == 'idle' && !of1 && ur.actPos == 'PRE_ATTACH_OF'", "tighten := active"),
  //   c("isExecuting", s"tighten == 'active'"),
  //   c("isFinished", s"tighten == 'active' && of1", "tighten := 'idle'")
  // )

  val getlftool = o("getLfTool", attr = SPAttributes("notInModel" -> true, "hasGoal" -> true))(
    c("pre", s"recu.robot_not_connected_to_tool && ur.actPos == 'PRE_ATTACH_OF'"),
    c("post", s"recu.robot_connected_to_lf_tool && ur.actPos == 'PRE_ATTACH_OF'")
  )

  val lfToEngine = o("putLfOnEngine", attr = SPAttributes("notInModel" -> true, "hasGoal" -> true))(
    c("pre", s"ur.actPos == 'PRE_ATTACH_OF' && lf == 'onMir'"),
    c("post", s"lf == 'onEngine'")
  )

  val getoftool = o("getOfTool", attr = SPAttributes("notInModel" -> true, "hasGoal" -> true))(
    c("pre", s"!ur.isAttachedOFMoveit && recu.robot_not_connected_to_tool && ur.actPos == 'PRE_ATTACH_OF'"),
    c("post", s"ur.isAttachedOFMoveit && recu.robot_connected_to_filter_tool && ur.actPos == 'PreAttachOFToolFarJOINTPose'")
  )

  val leaveoftool = o("leaveOfTool", attr = SPAttributes("notInModel" -> true, "hasGoal" -> true))(
    c("pre", s"recu.robot_connected_to_filter_tool && ur.actPos == 'PRE_ATTACH_OF'"),
    c("post", s"!ur.isAttachedOFMoveit && recu.robot_not_connected_to_tool && ur.actPos == 'PreAttachOFToolFarJOINTPose'")
  )


  val attachOF1 = o("attachOF1", attr = SPAttributes("notInModel" -> true, "hasGoal" -> true))(
    c("pre", s"!of1 && ur.actPos == 'PRE_ATTACH_OF' && recu.robot_connected_to_filter_tool"),
    c("post", s"of1 && ur.actPos == 'PRE_ATTACH_OF'")
  )

  val attachOF2 = o("attachOF2", attr = SPAttributes("notInModel" -> true, "hasGoal" -> true))(
    c("pre", s"!of2"),
    c("post", s"of2")
  )

  // instantiate abilities
  makeOps()

  // add resource bookings
  addBookings()


  val highLevelOps = List(getlftool, lfToEngine, getoftool, leaveoftool, attachOF1, attachOF2)
  // just build a simple sop to visualize the ability states
  // make a grid with four columns to utilize the space we have in the widget
  val grid = List(0,1,2,3).map(n=>operations.filterNot(o=>highLevelOps.exists(_==o.id)).sliding(4,4).flatMap(_.lift(n)).toList)
  sop("abilities", List(Parallel(grid.map(s=>Sequence(s.map(o=>SOP(o)))))))
  sop("operations", List(Sequence(highLevelOps.map(o=>SOP(o)))))

  exportNuXmv("ipsintegration.smv")
}
