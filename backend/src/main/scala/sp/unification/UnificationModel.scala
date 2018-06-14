package sp.unification

import sp.modelSupport._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler._

import sp.drivers.ROSFlatStateDriver

class UnificationModel extends ModelDSL {
  use("UR", new UR)
//  use("MiR", new MiR)

  // runner (TODO: for now runners take everything and must be on the top level of the model)
  runner("runner")

  // share a single driver for all ROS nodes
  driver("ROSdriver", ROSFlatStateDriver.driverType)
}

class MiR extends ModelDSL {
  use("pose", new MiRPose)
  use("mode", new MiRMode)
}

class MiRPose extends ModelDSL {
  def s(field: String) = dv(field, "ROSdriver", s"unification_roscontrol/MiRPoseUniToSP:/unification_roscontrol/mir_pose_unidriver_to_sp:$field")
  def p(field: String, rate: Int = 250) = dv(field, "ROSdriver", s"unification_roscontrol/MiRPoseSPToUni:/unification_roscontrol/mir_pose_sp_to_unidriver:$field:$rate")

  // pose state
  List("mir_pose_unidriver_got_msg_from_sp", "got_cmd_ref_pos", "mir_pose_unidriver_got_msg_from_mir_pose_smaster", "act_pos").foreach(s)

  // pose commands
  List("ref_pos")

  // abilities
  a("gotoKitting", List(),
    c("pre", "true", s"ref_pos := 'kitting'"),
    c("started", s"got_cmd_ref_pos == 'kitting'"), // note that we check the driver state
    c("post", s"act_pos == 'kitting'"),
    c("reset", "true"))

}

class MiRMode extends ModelDSL {
  def s(field: String) = dv(field, "ROSdriver", s"unification_roscontrol/MiRModeUniToSP:/unification_roscontrol/mir_mode_unidriver_to_sp:$field")
  def p(field: String, rate: Int = 250) = dv(field, "ROSdriver", s"unification_roscontrol/MiRModeSPToUni:/unification_roscontrol/mir_mode_sp_to_unidriver:$field:$rate")

  // mode state
  List("mir_mode_unidriver_got_msg_from_sp", "got_cmd_set_state_to_ready", "got_cmd_set_state_to_paused", "got_cmd_set_state_to_executing", "got_cmd_set_state_to_aborted", "mir_mode_unidriver_got_msg_from_mir_mode_smaster", "none", "starting", "shutting_down", "ready", "pause", "executing", "aborted", "completed", "docked", "docking", "emergency_stop", "manual_control", "error").foreach(s)

  // mode commands
  List("set_state_to_ready", "set_state_to_paused", "set_state_to_executing", "set_state_to_aborted").foreach(f=>p(f))

  a("setReady", List(),
    c("pre", "true", s"set_state_to_ready := true"),
    c("started", s"got_cmd_set_state_to_ready"), // note that we check the driver state
    c("post", "true"),
    c("reset", "true"))

  // blank list of things = take everything
  resource("URMode")
}

class UR extends ModelDSL {
  use("pose", new URPose)
//  use("mode", new URMode)
}

class URMode extends ModelDSL {
  def s(field: String) = dv(field, "ROSdriver", s"unification_roscontrol/URModeUniToSP:/unification_roscontrol/ur_mode_unidriver_to_sp:$field")
  def p(field: String, rate: Int = 250) = dv(field, "ROSdriver", s"unification_roscontrol/URModeSPToUni:/unification_roscontrol/ur_mode_sp_to_unidriver:$field:$rate")

  // mode state
  List("ur_mode_unidriver_got_msg_from_sp", "got_cmd_ur_activate_safeguard", "got_cmd_ur_disengage_safeguard", "got_cmd_ur_disengage_protective",
    "ur_mode_unidriver_got_msg_from_ur_mode_smaster", "normal", "reduced", "protective_stop", "recovery", "safeguard_stop", "system_emergency_stop", "robot_emergency_stop",
    "violation", "fault").foreach(s)

  // mode commands
  List("ur_activate_safeguard", "ur_disengage_safeguard", "ur_disengage_protective").foreach(f=>p(f))

  // blank list of things = take everything
  resource("URMode")
}

class URPose extends ModelDSL {
  // helpers for dvs - use the same sp name as the ros msg field
  def s(field: String) = dv(field, "ROSdriver", s"unification_roscontrol/URPoseUniToSP:/unification_roscontrol/ur_pose_unidriver_to_sp:$field")
  def p(field: String, rate: Int = 250) = dv(field, "ROSdriver", s"unification_roscontrol/URPoseSPToUni:/unification_roscontrol/ur_pose_sp_to_unidriver:$field:$rate")

  // pose state
  List("ur_pose_unidriver_got_msg_from_sp", "got_cmd_should_plan", "got_cmd_ref_pos", "ur_pose_unidriver_got_msg_from_ur_tcp_pose_smaster", "ur_pose_unidriver_got_msg_from_ur_joint_pose_smaster", "ur_pose_unidriver_got_msg_from_moveit_smaster", "act_pos", "executing", "planning").foreach(s)

  // pose commands
  List("should_plan", "ref_pos").foreach(f=>p(f))



  //



  // ur abilities
  List("URDummyPose1", "URDummyPose2", "URDummyPose3", "URDummyPose4").foreach { pose =>
    a("goto"+pose, List(),
      c("pre", "true", s"ref_pos := '$pose'"),
      c("started", s"got_cmd_ref_pos == '$pose' && executing"), // note that we check the driver state
      c("post", s"act_pos == '$pose' && !executing"),
      c("reset", "true"))
  }



  // add a sequence
  o("gotoURDummyPose2", c("pre", "ur_act_pos == 'URDummyPose1'"))
  o("gotoURDummyPose3", c("pre", "ur_act_pos == 'URDummyPose2'"))
  o("gotoURDummyPose4", c("pre", "ur_act_pos == 'URDummyPose3'"))

  // blank list of things = take everything
  resource("URPose")
}

object UnificationModel {
  def apply() = new UnificationModel
}
