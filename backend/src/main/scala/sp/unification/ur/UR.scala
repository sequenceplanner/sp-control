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

  // abilities
  List("URDummyPose1", "URDummyPose2", "URDummyPose3", "URDummyPose4").foreach { pose =>
    a("goto"+pose, List(),
      c("pre", "true", s"ref_pos := '$pose'"),
      c("started", s"got_cmd_ref_pos == '$pose' && executing"), // note that we check the driver state
      c("post", s"act_pos == '$pose' && !executing"),
      c("reset", "true", "got_cmd_ref_pos := 'ResetJOINT'"))
  }

  // variables needs to be explicit now
  v("act_pos", "URDummyPose1", List("URDummyPose1","URDummyPose2","URDummyPose3","URDummyPose4","unknown"))

  // add a dummy sequence
  o("gotoURDummyPose1")(
    c("pre", "act_pos == 'unknown' || act_pos == 'URDummyPose4'", "act_pos := URDummyPose1"),
    c("post", "act_pos == 'URDummyPose1'"))

  o("gotoURDummyPose2")(
    c("pre", "act_pos == 'URDummyPose1'", "act_pos := URDummyPose2"),
    c("post", "act_pos == 'URDummyPose2'"))

  o("gotoURDummyPose3")(
    c("pre", "act_pos == 'URDummyPose2'", "act_pos := URDummyPose3"),
    c("post", "act_pos == 'URDummyPose3'")
  )
  o("gotoURDummyPose4")(
    c("pre", "act_pos == 'URDummyPose3'", "act_pos := URDummyPose4"),
    c("post", "act_pos == 'URDummyPose4'")
  )



  // blank list of things = take everything
  resource("resource")
}
