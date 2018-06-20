package sp.unification

import sp.modelSupport._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler._

import sp.drivers.ROSFlatStateDriver
import sp.drivers.ROSHelpers

import unification_roscontrol._

class Atlas extends ModelDSL {
  use("atlas", new Atla)
}

class Atla extends ModelDSL with ROSSupport {
  reader("ROSdriver", "unification_roscontrol/AecuUniToSP", "/unification_roscontrol/aecu_unidriver_to_sp")
  writer("ROSdriver", "unification_roscontrol/AecuSPToUni", "/unification_roscontrol/aecu_sp_to_unidriver", 250)

  // abilities
  a("lift", List(),
    c("pre", "true", "activate_lift:=true", "activate_unload:=true"),
    c("started", "got_cmd_activate_lift && got_cmd_activate_unload"),
    c("post", "true"),
    c("reset", "true"))

  a("float", List(),
    c("pre", "true", "activate_lift:=false", "activate_unload:=true"),
    c("started", "!got_cmd_activate_lift && got_cmd_activate_unload"),
    c("post", "true"),
    c("reset", "true"))


  // blank list of things = take everything
  resource("resource")
}
