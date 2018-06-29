package sp.unification

import sp.modelSupport._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler._

import sp.drivers.ROSFlatStateDriver
import sp.drivers.ROSHelpers


class UR extends ModelDSL {
  use("pose", new URPose)
  use("mode", new URMode)
}

class URMode extends ModelDSL with ROSSupport {
  reader("URdriver", "unification_roscontrol/URModeUniToSP", "/unification_roscontrol/ur_mode_unidriver_to_sp")
  writer("URdriver", "unification_roscontrol/URModeSPToUni", "/unification_roscontrol/ur_mode_sp_to_unidriver", 250)

  // abilities
  a("doNotProtect", List(),
    c("pre", "true", s"ur_disengage_protective := true"),
    c("started", s"true"),
    c("post", "NOT protective_stop", s"ur_disengage_protective := false"),
    c("reset", "true"))

  // blank list of things = take everything
  resource("resource")
}

class URPose extends ModelDSL with ROSSupport {
  import UnificationModel._

  reader("URdriver", "unification_roscontrol/URPoseUniToSP", "/unification_roscontrol/ur_pose_unidriver_to_sp")
  writer("URdriver", "unification_roscontrol/URPoseSPToUni", "/unification_roscontrol/ur_pose_sp_to_unidriver", 1000)

  // abilities
  poses.foreach { pose =>
    a("goto_"+pose, List(),
      c("pre", s"true", s"ref_pos := '$pose'"),
      c("started", s"got_cmd_ref_pos == '$pose' && (executing || act_pos == '$pose')"),
      c("post", s"act_pos == '$pose'"),
      c("reset", "true"))
  }

  a("reset", List(),
    c("pre", "true", s"ref_pos := 'reset'"),
    c("started", s"true"),
    c("post", "got_cmd_ref_pos == 'reset'"),
    c("reset", "true"))

  a("continue", List(),
    c("pre", "true", s"ref_pos := 'reset'"),
    c("started", s"true", s"ref_pos := got_cmd_ref_pos"),
    c("post", "true"),
    c("reset", "true"))

  // blank list of things = take everything

  driver("URdriver", ROSFlatStateDriver.driverType)
  resource("resource")
}
