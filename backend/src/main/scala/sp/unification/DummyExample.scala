package sp.unification

import sp.devicehandler._
import sp.domain.Logic._
import sp.drivers.{ROSFlatStateDriver, URDriver}

class DummyTurtle(n: String) extends VDHelper {
  val name = n

  // state
  dv("currentPos", "driver","currentPos")
  dv("hasTool", "driver", "hasTool")

  // cmd
  dv("active", "driver", "active")
  dv("refPos", "driver", "refPos")

  // turtle abilities
  a("moveForward", List(),
    ac("pre", "true", "refPos := 10", "active := true"),
    ac("started", "currentPos != refPos"),
    ac("post", "currentPos == refPos"),
    ac("reset", "true"))

  a("moveBackward", List(),
    ac("pre", "true", "refPos := 0", "active := true"),
    ac("started", "currentPos != refPos"),
    ac("post", "currentPos == refPos"),
    ac("reset", "true"))

  // turtle operations
  o("moveForward",
    oc("pre", "currentPos == 0"),
    oc("post", "false"))

  o("moveBackward",
    oc("pre", "currentPos == 10"),
    oc("post", "false"))

  // drivers and resources
  driver("driver", URDriver.driverType)
  resource("resource") // blank list of things = take everything
}

class DummyTurtleModel(n: String) extends VDHelper {
  val name = n

  use(new DummyTurtle("DummyRB"))

  v("forceX")
  o("forceGoForward",
    oc("pre", "forceX"),
    oc("post", "false"), "DummyRB.moveForward")

  // runner
  r("turtlerunner", initState = Map("forceX" -> true))

}

object DummyTurtleModel {
  def apply(name: String = "DummyRB") = new DummyTurtleModel(name)
}
