package sp.unification

import sp.modelSupport._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler._

import sp.drivers.ROSFlatStateDriver

class UR extends ModelDSL {
  use("pose", new URPose)

  // runner (for now runners take everything and must be on the top level of the model)
  runner("runner")
}

class URPose extends ModelDSL {
  // UrSubscriber helper - use the same sp name as the ros msg field
  def us(field: String) = dv(field, "ROSdriver", s"unification_roscontrol/URPoseUniToSP:/unification_roscontrol/ur_pose_unidriver_to_sp:$field")

  List("ur_pose_unidriver_got_msg_from_sp", "got_cmd_should_plan", "got_cmd_ref_pos", "ur_pose_unidriver_got_msg_from_ur_pose_smaster",
    "act_pos", "executing", "planning").foreach(us)

  // UrPublisher helper - use the same sp name as the ros msg field
  def up(field: String, rate: Int = 250) = dv(field, "ROSdriver", s"unification_roscontrol/URPoseSPToUni:/unification//unification_roscontrol/ur_pose_sp_to_unidriver:$field:$rate")

  List("should_plan", "ref_pos").foreach(f=>up(f))



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

  // drivers and resources
  driver("ROSdriver", ROSFlatStateDriver.driverType)

  // blank list of things = take everything
  resource("resource")
}

object UR {
  def apply() = new UR
}
