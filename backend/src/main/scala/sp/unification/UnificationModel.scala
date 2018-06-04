package sp.unification

import sp.domain.Logic._
import sp.domain._
import sp.devicehandler._

import sp.drivers.ROSFlatStateDriver

class URModel(n: String) extends VDHelper {
  val name = n
  // ur robot state
  dv("ur_act_pos", "unification_roscontrol/URPose1:/unification/ur_pose_unidriver/state:actPos")
  dv("ur_executing", "unification_roscontrol/URPose1:/unification/ur_pose_unidriver/state:executing")
  dv("ur_ref_pos_driver", "unification_roscontrol/URPose1:/unification/ur_pose_unidriver/state:refPos")

  // ur robot commands
  dv("ur_ref_pos_sp", "unification_roscontrol/URPose2:/unification/ur_pose_unidriver/cmd:refPos:250")

  // ur abilities
  List("URDummyPose1", "URDummyPose2", "URDummyPose3", "URDummyPose4").foreach { pose =>
    a("goto"+pose, List(),
      ac("pre", "true", s"ur_ref_pos_sp := '$pose'"),// s"ur_act_pos != '$pose'", s"ur_ref_pos_sp := '$pose'"),
      ac("started", s"ur_ref_pos_driver == '$pose' && ur_executing"), // note that we check the driver state
      ac("post", s"ur_act_pos == '$pose' && !ur_executing"),
      ac("reset", "true"))
  }
  // leave all abilities unchanged except for movetopose2
  //  o("gotoURDummyPose2", p("pre", "ur_act_pos == 'URDummyPose1'"))

  // drivers and resources
  val driverID = ID.newID
  val driverSetup = SPAttributes("identifiers" -> driverMapper(driverID).map(_.driverIdentifier))
  val driver = VD.Driver("URVD", driverID, ROSFlatStateDriver.driverType, driverSetup)
  val resource = VD.Resource("UR", ID.newID, dthings.values.map(_.id).toSet, driverMapper(driverID).toList, SPAttributes())
}

object URModel {
  def apply(name: String = "UR") = new URModel(name)
}
