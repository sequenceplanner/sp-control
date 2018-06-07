package sp.unification

import sp.domain.Logic._
import sp.domain._
import sp.devicehandler._

import sp.drivers.ROSFlatStateDriver

class URModel(n: String) extends VDHelper {
  val name = n
  // ur robot state
  dv("ur_act_pos", "driver", "unification_roscontrol/URPose1:/unification/ur_pose_unidriver/state:actPos")
  dv("ur_executing", "driver", "unification_roscontrol/URPose1:/unification/ur_pose_unidriver/state:executing")
  dv("ur_ref_pos_driver", "driver", "unification_roscontrol/URPose1:/unification/ur_pose_unidriver/state:refPos")

  // ur robot commands
  dv("ur_ref_pos_sp", "driver", "unification_roscontrol/URPose2:/unification/ur_pose_unidriver/cmd:refPos:250")

  // ur abilities
  List("URDummyPose1", "URDummyPose2", "URDummyPose3", "URDummyPose4").foreach { pose =>
    a("goto"+pose, List(),
      ac("pre", "true", s"ur_ref_pos_sp := '$pose'"),
      ac("started", s"ur_ref_pos_driver == '$pose' && ur_executing"), // note that we check the driver state
      ac("post", s"ur_act_pos == '$pose' && !ur_executing"),
      ac("reset", "true"))
  }
  // add a sequence
  o("gotoURDummyPose2", oc("pre", "ur_act_pos == 'URDummyPose1'"))
  o("gotoURDummyPose3", oc("pre", "ur_act_pos == 'URDummyPose2'"))
  o("gotoURDummyPose4", oc("pre", "ur_act_pos == 'URDummyPose3'"))

  // drivers and resources
  driver("driver", ROSFlatStateDriver.driverType)
  resource("resource") // blank list of things = take everything

  // runner
  r("runner", initState = Map("ur_act_pos" -> "unknown?"))
}

object URModel {
  def apply(name: String = "UR") = new URModel(name)
}
