package sp.models.unification.minimal

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
    "HomeJOINTPose",
    "PreHomeJOINTPose",
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
    // bolt tightening
    "AboveEngineTCPPose",
    "FarAboveBoltPair1TCPPose",
    "CloseAboveBoltPair1TCPPose",
    "AtBoltPair1TCPPose",
    "FarAboveBoltPair2TCPPose",
    "CloseAboveBoltPair2TCPPose",
    "AtBoltPair2TCPPose",
    "FarAboveBoltPair3TCPPose",
    "CloseAboveBoltPair3TCPPose",
    "AtBoltPair3TCPPose",
    "FarAboveBoltPair4TCPPose",
    "CloseAboveBoltPair4TCPPose",
    "AtBoltPair4TCPPose",
    "FarAboveBoltPair5TCPPose",
    "CloseAboveBoltPair5TCPPose",
    "AtBoltPair5TCPPose",
    "FarAboveBoltPair6TCPPose",
    "CloseAboveBoltPair6TCPPose",
    "AtBoltPair6TCPPose",
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
    case (_, target) if target.contains("JOINTPose") => ("JOINT", "MOVEJ")
    case (_, target) if target.contains("TCPPose") => ("TCP", "MOVEL")

    case _ => // println(s"DEFAULT CASE for $source -> $target");
      ("JOINT", "PLANNED")
  }

  // allowed movements without the LF tool
  // PreAttachLFToolFarJOINT from HomeJOINTPose, PreAttachLFToolCloseTCP
  // PreAttachLFToolCloseTCP from PreAttachLFToolFarJOINT, AttachLFToolTCP,
  // AttachLFToolTCP from PreAttachLFToolCloseTCP,
  // AAPRLFToolTCP from List()
  val LFToolPoses = List("PreAttachLFToolFarJOINTPose", "PreAttachLFToolCloseTCPPose", "AttachLFToolTCPPose", "AAPRLFToolTCPPose")
  val moveMapLFToolNoTool = Map(
    "PreAttachLFToolFarJOINTPose" -> List("HomeJOINTPose", "PreAttachLFToolCloseTCPPose"),
    "PreAttachLFToolCloseTCPPose" -> List("PreAttachLFToolFarJOINTPose", "AttachLFToolTCPPose"),
    "AttachLFToolTCPPose" -> List("PreAttachLFToolCloseTCPPose"),
    "AAPRLFToolTCPPose" -> List()
  )

  // allowed movements with the LF tool
  // PreAttachLFToolFarJOINT from HomeJOINTPose, AAPRLFToolTCP
  // PreAttachLFToolCloseTCP from List(),
  // AttachLFToolTCP from AAPRLFToolTCP,
  // AAPRLFToolTCP from PreAttachLFToolFarJOINT with gripper open
  val moveMapLFToolWithTool = Map(
    "PreAttachLFToolFarJOINTPose" -> List("HomeJOINTPose", "AAPRLFToolTCPPose"),
    "PreAttachLFToolCloseTCPPose" -> List(),
    "AttachLFToolTCPPose" -> List("AAPRLFToolTCPPose"),
    "AAPRLFToolTCPPose" -> List("AttachLFToolTCPPose", "PreAttachLFToolFarJOINTPose") // with gripper open
  )

  val moveMapLFOp = Map(
    "HomeJOINTPose" -> List("PreAttachLFToolFarJOINTPose", "LFOperationMidpoint1JOINTPose"),
    "LFOperationMidpoint1JOINTPose" -> List("HomeJOINTPose", "PreAttachLFToolFarJOINTPose", "LFOperationMidpoint2JOINTPose"),
    "LFOperationMidpoint2JOINTPose" -> List("LFOperationMidpoint1JOINTPose","LFOperationMidpoint3JOINTPose"),
    "LFOperationMidpoint3JOINTPose" -> List("LFOperationMidpoint2JOINTPose","LFOperationMidpoint4JOINTPose"),
    "LFOperationMidpoint4JOINTPose" -> List("LFOperationMidpoint3JOINTPose", "LFOperationMidpoint5JOINTPose"),
    "LFOperationMidpoint5JOINTPose" -> List("LFOperationMidpoint4JOINTPose")
  )
}


class UR(override val system: ActorSystem) extends ROSResource {
  val actPos = i("actPos", UR.initialState, UR.poses.map(SPValue(_)))
  // val from = i("from", UR.initialState, UR.poses.map(SPValue(_)))
  // val to = i("to", UR.initialState, UR.poses.map(SPValue(_)))

  val moving = i("moving", false)
  val prevPos = i("prevPos", UR.initialState, UR.poses.map(SPValue(_))) // , Set(), SPAttributes("input" -> true, "notInModel" -> true))

  val subMapping = stringToIDMapper(Map("moving" -> moving, "actual_pose" -> actPos, "previous_pose" -> prevPos)) //, "from" -> from, "to" -> to))

  val prevMapping = Flow[Map[String, SPValue]].map{ state =>
    state ++ state.get("actual_pose").toList.filter(_!=SPValue("UNKNOWN")).filterNot(_.as[String].contains("moving")).map(spval => "previous_pose" -> spval)
  }

  val actPosMapping = Flow[Map[String, SPValue]].map{ state =>
    state.map { case (k,v) if k == "actual_pose" => if(!UR.poses.contains(v.as[String])) (k, SPValue("UNKNOWN")) else (k,v)
      case (k,v) => (k,v)
    }
  }

  // val fromToMapping = Flow[Map[String, SPValue]].map{ state =>
  //   val x = state.get("actual_pose").flatMap { str =>
  //     val s = str.as[String]
  //     val r = s.split("moving from ").lift(1).flatMap{s =>
  //       val x = s.split(" to "); x.lift(0).flatMap(y=>x.lift(1).map(z=>(y,z)))
  //     }

  //     r.map{ x => Map("from" -> SPValue(x._1), "to" -> SPValue(x._2)) }
  //   }.getOrElse(Map())
  //   state ++ x
  // }

  // hack to separate act pos into from and to
  //actual_pose: moving from HomeJOINTPose to PreAttachLFToolFarJOINTPose
  //actual_pose: moving from UNKNOWN to HomeJOINTPose

  val subFlow = prevMapping.via(actPosMapping).via(subMapping)
  subscribe("/unification_roscontrol/ur_TARS_pose_unidriver_uni_to_sp", "unification_ros2_messages/URPoseUniToSP", subFlow)

  val refPos = o("refPos", UR.initialState, UR.poses.map(SPValue(_)))
  val pubMapping = IDToStringMapper(Map(refPos -> "pose_name"))

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

  publish("/unification_roscontrol/ur_pose_unidriver_sp_to_uni", "unification_ros2_messages/URPoseSPToUni", None, pubFlow)

  a("moveToPos", List(refPos))(
    c("pre", "!moving"),
    c("startEffect", "true", "moving := true", "actPos := 'UNKNOWN'"),
    c("isExecuting", "moving || actPos != refPos"),
    c("immediateExecutingEffect", "true", "moving := false", "actPos := refPos", "prevPos := refPos"),
    c("isFinished", "!moving && actPos == refPos"),
    c("reset", "true"))

}


class AECU(override val system: ActorSystem) extends ROSResource {
  // bool tool_is_running_reverse
  // bool tool_is_in_alarm
  // bool positioned_at_home_station
  // bool operating_position
  // bool pre_home_position
  // bool unclear_position
  val ptr = i("programmed_torque_reached", false)
  val home = i("positioned_at_home_station", true)

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

  publish("/unification_roscontrol/aecu_sp_to_uni", "unification_ros2_messages/AecuSPToUni", None, pubMapping)

  v("nutter", false)
  a("startToolForward", List())(
    c("pre", "set_tool_idle && !run_tool_forward", "run_tool_forward:=true", "set_tool_idle := false"),
    c("isExecuting", "run_tool_forward && !set_tool_idle && !nutter"),
    c("executingEffect", "true", "programmed_torque_reached := false"),
    c("isFinished", "!programmed_torque_reached", "nutter := true"),
  )

  a("float", List())(
    c("pre", "!activate_unload", "activate_unload:=true", "activate_lift := false"),
    c("startEffect", "true", "positioned_at_home_station := false"),
    c("isFinished", "activate_unload && !activate_lift"),
  )

  a("lift", List())(
    c("pre", "!activate_lift", "activate_unload:=false", "activate_lift := true"),
    c("isExecuting", "!activate_unload && activate_lift && !positioned_at_home_station"),
    c("executingEffect", "true", "positioned_at_home_station := true"),
    c("isFinished", "!activate_unload && activate_lift && positioned_at_home_station"),
  )
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
  val atlastool = i("robot_connected_to_atlas_tool", false)
  val oftool = i("robot_connected_to_filter_tool", false)
  val rsp_is_locked = i("rsp_is_locked", false)
  val gripper_is_closed = i("gripper_is_closed", false)

  val subMapping = createInputMappingFromMessageType("unification_ros2_messages/RecuUniToSP")
  subscribe("/unification_roscontrol/recu_uni_to_sp", "unification_ros2_messages/RecuUniToSP", subMapping)

  val lock_rsp = o("lock_rsp", false)
  val unlock_rsp = o("unlock_rsp", false)
  val open_gripper = o("open_gripper", false)
  val close_gripper = o("close_gripper", false)

  val pubMapping = createOutputMappingFromMessageType("unification_ros2_messages/RecuSPToUni")
  publish("/unification_roscontrol/recu_sp_to_uni", "unification_ros2_messages/RecuSPToUni", None, pubMapping)

  a("lock")(
    c("pre", "!rsp_is_locked", "lock_rsp := true", "unlock_rsp := false", "open_gripper := false", "close_gripper := false"),
    c("isExecuting", "!rsp_is_locked && lock_rsp && !unlock_rsp && !open_gripper && !close_gripper"),
    c("executingEffect", "true", "rsp_is_locked := true"),
    c("isFinished", "rsp_is_locked == true"),
    c("reset", "true")
  )

  a("unlock")(
    c("pre", "rsp_is_locked", "lock_rsp := false", "unlock_rsp := true", "open_gripper := false", "close_gripper := false"),
    c("isExecuting", "rsp_is_locked && !lock_rsp && unlock_rsp && !open_gripper && !close_gripper"),
    c("executingEffect", "true", "rsp_is_locked := false"),
    c("isFinished", "rsp_is_locked == false"),
    c("reset", "true")
  )

  a("open")(
    c("pre", "robot_connected_to_lf_tool && rsp_is_locked && gripper_is_closed", "open_gripper := true", "close_gripper := false", "lock_rsp := false", "unlock_rsp := false"),
    c("isExecuting", "robot_connected_to_lf_tool && rsp_is_locked && gripper_is_closed && !lock_rsp && !unlock_rsp && open_gripper && !close_gripper"),
    c("executingEffect", "true", "gripper_is_closed := false"),
    c("isFinished", "gripper_is_closed == false"),
    c("reset", "true")
  )

  a("close")(
    c("pre", "robot_connected_to_lf_tool && rsp_is_locked && !gripper_is_closed", "open_gripper := false", "close_gripper := true", "lock_rsp := false", "unlock_rsp := false"),
    c("isExecuting", "robot_connected_to_lf_tool && rsp_is_locked && !gripper_is_closed && !lock_rsp && !unlock_rsp && !open_gripper && close_gripper"),
    c("executingEffect", "true", "gripper_is_closed := true"),
    c("isFinished", "gripper_is_closed == true"),
    c("reset", "true")
  )

}


class NewModel(override val system: ActorSystem) extends MiniModel {
//  use("aecu", new AECU(system))
  use("recu", new RECU(system))
  use("ur", new UR(system))

  // val startMotor = o(s"aecu.startToolForward", "aecu.startToolForward", "aecu")()

  // v(s"tightened", false)
  // o(s"watchForTightened", attr = SPAttributes("ability" -> "yes"))(
  //   c("isExecuting", s"aecu.nutter && aecu.activate_unload && aecu.run_tool_forward && !aecu.set_tool_idle && !aecu.programmed_torque_reached"),
  //   c("executingEffect", "true", "aecu.programmed_torque_reached := true"),
  //   c("isFinished", s"aecu.nutter && aecu.activate_unload && aecu.programmed_torque_reached", s"tightened := true", "aecu.run_tool_forward := false", "aecu.set_tool_idle := true", "aecu.nutter := false")
  // )

  // o("aecu.float", "aecu.float", "aecu")()
  // o("aecu.lift", "aecu.lift", "aecu")()

  // val runTwinSpin = o("runTwinSpin", attr = SPAttributes("notInModel" -> true, "hasGoal" -> true, "isa" -> "operation"))(
  //   c("pre", s"aecu.set_tool_idle && !aecu.positioned_at_home_station"),
  //   c("post", s"tightened")
  // )

  // val goHome = o("goHome", attr = SPAttributes("notInModel" -> true, "hasGoal" -> true, "isa" -> "operation"))(
  //   c("pre", s"tightened"),
  //   c("post", s"aecu.positioned_at_home_station")
  // )


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

  val lfToolPoses = UR.moveMapLFToolNoTool.keySet ++ UR.moveMapLFToolWithTool.keySet ++ UR.moveMapLFOp.keySet
  lfToolPoses.foreach { p =>
    val withoutTool = UR.moveMapLFToolNoTool.get(p).map{source =>
      val pose = if(source.isEmpty) "false" else s"(${source.map(s=>s"ur.actPos == '$s'").mkString("||")})"
      s"((recu.robot_not_connected_to_tool || ur.actPos == 'AttachLFToolTCPPose') && !recu.rsp_is_locked) && $pose"}.getOrElse("true")
    val withTool = UR.moveMapLFToolWithTool.get(p).map{source =>
      val pose = if(source.isEmpty) "false" else s"(${source.map(s=>s"ur.actPos == '$s'").mkString("||")})"
      s"(recu.robot_connected_to_lf_tool && recu.rsp_is_locked) && $pose"}.getOrElse("true")
    val lftool = c("pre", s"($withoutTool || $withTool)")

    o(s"ur.goto$p", "ur.moveToPos", "ur")(
      c("pre", s"ur.actPos != '$p' && ur.refPos != '$p'", s"ur.refPos := '$p'"),
      lftool
    )
  }

  // moving into position for LF magic operation
  UR.moveMapLFOp.foreach { case (target, sources) =>
    val sourcePose = if(sources.isEmpty) "false" else s"(${sources.map(s=>s"ur.actPos == '$s'").mkString("||")})"
//    val hasTool = "recu.robot_connected_to_lf_tool && recu.rsp_is_locked"
    o(s"ur.goto$target")(c("pre", s"$sourcePose"))    ///  && $hasTool
  }

  // disable this move -- performed by the ur-script
  o(s"ur.gotoAfterLFOperationJOINTPose")(c("pre", "false"))

  // execute "lf magic" ur-script
  v("lf", "onMir", List("onMir", "onEngine"))
  v("doingLFMagic", false) // no feedback for now, fake it
  o("ur.lfmagic", bookings = Set("ur"), attr = SPAttributes("ability" -> "yes"))(
    c("pre", s"recu.robot_connected_to_lf_tool && ur.actPos == 'LFOperationMidpoint5JOINTPose'", "doingLFMagic := true", "ur.refPos := 'UNKNOWN'"),
    c("isExecuting", s"doingLFMagic"),
    c("executingEffect", "true", "ur.actPos := 'AfterLFOperationJOINTPose'"),
    c("isFinished", s"doingLFMagic && ur.actPos == 'AfterLFOperationJOINTPose'", "doingLFMagic := false", "lf := onEngine"),
  )

  // gripper needs to be opened/closed during some moves
  o("recu.open", "recu.open", "ur")()
  o("recu.close", "recu.close", "ur")()

  val unlockRspWhenNotHoldingATool = o("unlockRSP", "recu.unlock", "ur")(
    c("pre", s"recu.robot_not_connected_to_tool"),
  )

  val unlockRSPWhenLeavingLFTool = o("lfDetach.unlockRSP", "recu.unlock", "ur")(
    c("pre", "recu.robot_connected_to_lf_tool && recu.gripper_is_closed && ur.actPos == 'AttachLFToolTCPPose'"),
  )

  // recu.robot_connected_to_lf_tool this sensor triggers before we lock the rsp. so wait for that then attach
  val lockRspToConnectLFTool = o("lfAttach.lockRSP", "recu.lock", "ur")(
    c("pre", s"recu.robot_connected_to_lf_tool && ur.actPos == 'AttachLFToolTCPPose'"),
  )


  o(s"ur.gotoAAPRLFToolTCPPose")(c("pre", "(recu.robot_connected_to_lf_tool && recu.rsp_is_locked && ((ur.prevPos == 'AttachLFToolTCPPose' && recu.gripper_is_closed) || (ur.prevPos != 'AttachLFToolTCPPose' && !recu.gripper_is_closed))) || (!recu.rsp_is_locked && recu.robot_not_connected_to_tool)"))

  o(s"ur.gotoPreAttachLFToolFarJOINTPose")(c("pre", "(recu.robot_connected_to_lf_tool && recu.rsp_is_locked && !recu.gripper_is_closed) || (!recu.rsp_is_locked && recu.robot_not_connected_to_tool)"))

  o(s"ur.gotoAttachLFToolTCPPose")(
    c("pre", "(recu.robot_connected_to_lf_tool && recu.rsp_is_locked && recu.gripper_is_closed) || (!recu.rsp_is_locked && recu.robot_not_connected_to_tool)"),
    c("executingEffect", "true", "recu.robot_connected_to_lf_tool := true"),
    c("isFinished", s"recu.robot_connected_to_lf_tool"),
  )

  val getLFTool = o("getLFTool", attr = SPAttributes("notInModel" -> true, "hasGoal" -> true, "isa" -> "operation"))(
    c("pre", s"recu.robot_not_connected_to_tool && ur.actPos == 'PreAttachLFToolFarJOINTPose'"),   // HomeJOINTPose
    c("post", s"recu.robot_connected_to_lf_tool && ur.actPos == 'PreAttachLFToolFarJOINTPose'")
  )

  val leaveLFTool = o("leaveLFTool", attr = SPAttributes("notInModel" -> true, "hasGoal" -> true, "isa" -> "operation"))(
    c("pre", s"recu.robot_connected_to_lf_tool && ur.actPos == 'PreAttachLFToolFarJOINTPose'"),   // HomeJOINTPose
    c("post", s"recu.robot_not_connected_to_tool && ur.actPos == 'PreAttachLFToolFarJOINTPose'")
  )

  /// to solve the sensor "race" at the attach pose we need to also allow the sensor to be triggered at that pose and not just during "transit" to it.

  // a bit complicated because sensors deactivated only when the robot is moving...
  // we need to think about immediate vs eventual effects. or "positive" and "negative" effects if you will.
  // quite interesting....
  o(s"watchForToolRelease", attr = SPAttributes("ability" -> "yes"))(
    c("isExecuting", s"ur.prevPos == 'AttachLFToolTCPPose' && (ur.refPos == 'PreAttachLFToolFarJOINTPose' || ur.refPos == 'PreAttachLFToolCloseTCPPose') && !recu.rsp_is_locked && recu.robot_connected_to_lf_tool"),
    c("immediateExecutingEffect", "true", "recu.robot_connected_to_lf_tool := false", "recu.robot_not_connected_to_tool := true")
  )

  // fix for the fact that the effect from move to attach may happen after the robot has finished moving.
  // o(s"watchForToolAttach", attr = SPAttributes("ability" -> "yes"))(
  //   c("isExecuting", s"ur.prevPos == 'AttachLFToolTCPPose' && ur.actPos == 'AttachLFToolTCPPose'"),
  //   c("executingEffect", "true", "recu.robot_connected_to_lf_tool := true", "recu.robot_not_connected_to_tool := false")
  // )


  // instantiate abilities
  println("make ops")
  makeOps()

  // add resource bookings
  println("add booking")
  addBookings()


  // just build a simple sop to visualize the ability states
  // make a grid with four columns to utilize the space we have in the widget
  val highLevelOps = operations.filter(_.attributes.getAs[String]("isa").contains("operation"))
  val abilities = operations.filterNot(_.attributes.getAs[String]("isa").contains("operation"))
  val grid = List(0,1,2,3).map(n=>abilities.sliding(4,4).flatMap(_.lift(n)).toList)
  sop("abilities", List(Parallel(grid.map(s=>Sequence(s.map(o=>SOP(o)))))))
  sop("operations", List(Sequence(highLevelOps.map(o=>SOP(o)))))

  exportNuXmv("minimal.smv")
}
