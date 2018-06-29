package sp.unification

import sp.modelSupport._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler._

import sp.drivers.ROSFlatStateDriver
import sp.drivers.ROSHelpers

import unification_roscontrol._

class RECU extends ModelDSL with ROSSupport {
  reader("RECUDriver", "unification_roscontrol/RecuUniToSP", "/unification_roscontrol/recu_unidriver_to_sp")
  writer("RECUDriver", "unification_roscontrol/RecuSPToUni", "/unification_roscontrol/recu_sp_to_unidriver", 1000)

  driver("RECUDriver", ROSFlatStateDriver.driverType)

  a("lock_rsp", List(),
    c("pre", "true", "lock_rsp:=true","unlock_rsp:=false","open_gripper:=false","close_gripper:=false"),
    c("started", "got_cmd_lock_rsp"),
    c("post", "true"),
    c("reset", "true"))

  a("unlock_rsp", List(),
    c("pre", "true", "lock_rsp:=false","unlock_rsp:=true","open_gripper:=false","close_gripper:=false"),
    c("started", "got_cmd_unlock_rsp"),
    c("post", "true"),
    c("reset", "true"))

  a("open_gripper", List(),
    c("pre", "true", "lock_rsp:=false","unlock_rsp:=false","open_gripper:=true","close_gripper:=false"),
    c("started", "got_cmd_open_gripper"),
    c("post", "true"),
    c("reset", "true"))

  a("close_gripper", List(),
    c("pre", "true", "lock_rsp:=false","unlock_rsp:=false","open_gripper:=false","close_gripper:=true"),
    c("started", "got_cmd_close_gripper"),
    c("post", "true"),
    c("reset", "true"))

  resource("RECU")
}

class HECU extends ModelDSL with ROSSupport {
  reader("HECUDriver", "unification_roscontrol/HecuUniToSP", "/unification_roscontrol/hecu_unidriver_to_sp")
  // this doesnt have a writer
  driver("HECUDriver", ROSFlatStateDriver.driverType)
  resource("HECU")
}

class Atlas extends ModelDSL with ROSSupport {
  reader("AtlasDriver", "unification_roscontrol/AecuUniToSP", "/unification_roscontrol/aecu_unidriver_to_sp")
  writer("AtlasDriver", "unification_roscontrol/AecuSPToUni", "/unification_roscontrol/aecu_sp_to_unidriver", 1000)

  // abilities
  a("lift", List(),
    c("pre", "true", "activate_lift:=true", "activate_unload:=false"),
    c("started", "got_cmd_activate_lift"),
    c("post", "true"),
    c("reset", "true"))

  a("float", List(),
    c("pre", "true", "activate_lift:=false", "activate_unload:=true"),
    c("started", "!got_cmd_activate_lift && got_cmd_activate_unload"),
    c("post", "true"),
    c("reset", "true"))

  a("startToolForward", List(),
    c("pre", "true", "run_tool_forward:=true", "set_tool_idle := false"),
    c("started", "got_cmd_run_tool_forward"),
    c("post", "true"),
    c("reset", "true"))

  a("stopToolForward", List(),
    c("pre", "true", "run_tool_forward:=false", "set_tool_idle := true"),
    c("started", "!got_cmd_run_tool_forward"),
    c("post", "true"),
    c("reset", "true"))

  driver("AtlasDriver", ROSFlatStateDriver.driverType)
  // blank list of things = take everything
  resource("resource")
}
