package sp.unification

import sp.modelSupport._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler._

import sp.drivers.ROSFlatStateDriver
import sp.drivers.ROSHelpers


trait ROSSupport extends ModelDSL {
  def writer(driver: String, messageType: String, topic: String, rate: Int) = {
    val emptyMsg = ROSHelpers.createROSMsg(messageType).get // puke if we mis-spell
    val attr = ROSHelpers.ROSMsgToSPAttributes(emptyMsg).get // puke if we can't parse
    attr.value.foreach {
      case (field, nested: SPAttributes) =>
      // TODO: later
      case (field, v) =>
        val ident = messageType + ":" + topic + ":" + field + ":" + rate
        dv(field, driver, ident, WriteOnly)
    }
  }
  def reader(driver: String, messageType: String, topic: String) = {
    val emptyMsg = ROSHelpers.createROSMsg(messageType).get // puke if we mis-spell
    val attr = ROSHelpers.ROSMsgToSPAttributes(emptyMsg).get // puke if we can't parse
    attr.value.foreach {
      case (field, nested: SPAttributes) =>
      // TODO: later
      case (field, v) =>
        val ident = messageType + ":" + topic + ":" + field
        dv(field, driver, ident, ReadOnly)
    }
  }
}

class UnificationModel extends ModelDSL {
  use("UR", new UR)
  use("MiR", new MiR)

  // runner (TODO: for now runners take everything and must be on the top level of the model)
  runner("runner")

  // share a single driver for all ROS nodes
  driver("ROSdriver", ROSFlatStateDriver.driverType)
}

class MiR extends ModelDSL {
  use("pose", new MiRPose)
  use("mode", new MiRMode)
}

class MiRPose extends ModelDSL with ROSSupport {
  reader("ROSdriver", "unification_roscontrol/MiRPoseUniToSP", "/unification_roscontrol/mir_pose_unidriver_to_sp")
  writer("ROSdriver", "unification_roscontrol/MiRPoseSPToUni", "/unification_roscontrol/mir_pose_sp_to_unidriver", 250)

  // abilities
  a("gotoKitting", List(),
    c("pre", "true", s"ref_pos := 'kitting'"),
    c("started", s"got_cmd_ref_pos == 'kitting'"),
    c("post", s"act_pos == 'kitting'"),
    c("reset", "true"))

  resource("resource")
}

class MiRMode extends ModelDSL with ROSSupport {
  reader("ROSdriver", "unification_roscontrol/MiRModeUniToSP", "/unification_roscontrol/mir_mode_unidriver_to_sp")
  writer("ROSdriver", "unification_roscontrol/MiRModeSPToUni", "/unification_roscontrol/mir_mode_sp_to_unidriver", 250)

  // abilities
  a("setReady", List(),
    c("pre", "true", s"set_state_to_ready := true"),
    c("started", s"got_cmd_set_state_to_ready"), // note that we check the driver state
    c("post", "true"),
    c("reset", "true"))

  // blank list of things = take everything
  resource("resource")
}

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
      c("reset", "true", "got_cmd_ref_pos == ResetJOINT"))
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

object UnificationModel {
  def apply() = new UnificationModel
}
