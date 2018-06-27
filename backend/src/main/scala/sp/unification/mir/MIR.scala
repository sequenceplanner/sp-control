package sp.unification

import sp.modelSupport._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler._

import sp.drivers.ROSFlatStateDriver
import sp.drivers.ROSHelpers


class MiR extends ModelDSL {
  use("pose", new MiRPose)
  //use("mode", new MiRMode)
}

class MiRPose extends ModelDSL with ROSSupport {
  reader("MIRdriver", "unification_roscontrol/MiRPoseUniToSP", "/unification_roscontrol/mir_pose_unidriver_to_sp")
  writer("MIRdriver", "unification_roscontrol/MiRPoseSPToUni", "/unification_roscontrol/mir_pose_sp_to_unidriver", 250)

  // abilities
  a("goto_Assembly", List(),
    c("pre", "true", s"ref_pos := 'assembly'"),
    c("started", s"got_cmd_ref_pos == 'assembly'"),
    c("post", s"act_pos == 'assembly'"),
    c("reset", "true"))

  a("reset", List(),
    c("pre", "true", s"ref_pos := 'reset'"),
    c("started", s"true"),
    c("post", "got_cmd_ref_pos == 'reset'"),
    c("reset", "true"))

  driver("MIRdriver", ROSFlatStateDriver.driverType)
  resource("resource")
}

class MiRMode extends ModelDSL with ROSSupport {
  reader("MIRdriver", "unification_roscontrol/MiRModeUniToSP", "/unification_roscontrol/mir_mode_unidriver_to_sp")
  writer("MIRdriver", "unification_roscontrol/MiRModeSPToUni", "/unification_roscontrol/mir_mode_sp_to_unidriver", 250)

  // abilities
  a("setReady", List(),
    c("pre", "true", s"set_state_to_ready := true"),
    c("started", s"got_cmd_set_state_to_ready"), // note that we check the driver state
    c("post", "true"),
    c("reset", "true"))

  // blank list of things = take everything
  resource("resource")
}
