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
  reader("ROSdriver", "unification_roscontrol/URModeUniToSP", "/unification_roscontrol/ur_mode_unidriver_to_sp")
  writer("ROSdriver", "unification_roscontrol/URModeSPToUni", "/unification_roscontrol/ur_mode_sp_to_unidriver", 250)

  // abilities

  // blank list of things = take everything
  resource("resource")
}

class URPose extends ModelDSL with ROSSupport {
  reader("ROSdriver", "unification_roscontrol/URPoseUniToSP", "/unification_roscontrol/ur_pose_unidriver_to_sp")
  writer("ROSdriver", "unification_roscontrol/URPoseSPToUni", "/unification_roscontrol/ur_pose_sp_to_unidriver", 250)

  val poses = List("HomeJOINT", "PreAttachLFToolFarJOINT", "PreFindEngineJOINT")
  // abilities
  poses.foreach { pose =>
    a("goto"+pose, List(),
      c("pre", "true", s"ref_pos := '$pose'"),
      c("started", s"got_cmd_ref_pos == '$pose' && executing"), // note that we check the driver state
      c("post", s"!executing"), // act_pos == '$pose' &&
      c("reset", "true", "ref_pos := 'ResetJOINT'"))
  }

  // blank list of things = take everything
  resource("resource")
}
