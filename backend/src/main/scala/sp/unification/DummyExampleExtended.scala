package sp.unification

import sp.domain.Logic._
import sp.drivers.URDriver
import sp.modelSupport._

class DummyRobot extends ModelDSL {

  dv("currentPos", "driver","currentPos")
  dv("hasTool", "driver", "hasTool")

  // cmd
  dv("active", "driver", "active")
  dv("refPos", "driver", "refPos")

  // Klurigt med integer som domän...
  v("refPos", "init", List("init", "failed"))
  v("currentPos", "init", List("stop", "stopFailed"))

  a("moveToPos", List("refPos"),
    c("pre", "true", "active := true"),
    c("started", "currentPos != refPos"),
    c("post", "currentPos == refPos"),
    c("reset", "true"))


  driver("driver", URDriver.driverType)
  resource("resource") // blank list of things = take everything
}

class DummyExampleExtended extends ModelDSL {
  use("R1", new DummyRobot)
  use("R2", new DummyRobot)
  use("R3", new DummyRobot)

  // Domänen kanske skall vara SPValue?
  v("part1", "false", List("false", "true"))
  v("part2", "false", List("false", "true"))
  v("part3", "false", List("false", "true"))

  o("R1_place1", "R1.moveToPos")(
    c("pre", "!part1 && !part3", "R1.refPos = 10"),
    c("post", "false", "part1 = true"),
    c("reset", "true")
  )

  o("R1_remove1", "R1.moveToPos")(
    c("pre", "part1 && part3", "R1.refPos = 20"),
    c("post", "false", "part1 = false"),
    c("reset", "true")
  )

  o("R2_place2", "R2.moveToPos")(
    c("pre", "!part2 && !part3", "R2.refPos = 10"),
    c("post", "false", "part2 = true"),
    c("reset", "true")
  )

  o("R2_remove1", "R2.moveToPos")(
    c("pre", "part2 && part3", "R2.refPos = 20"),
    c("post", "false", "part2 = false"),
    c("reset", "true")
  )

  o("R3_place3", "R3.moveToPos")(
    c("pre", "!part3 && part1 && part2", "R3.refPos = 10"),
    c("post", "false", "part3 = true"),
    c("reset", "true")
  )

  o("R3_remove3", "R3.moveToPos")(
    c("pre", "part3 && !part1 && !part2", "R3.refPos = 0"),
    c("post", "false", "part3 = false"),
    c("reset", "true")
  )


  runner("extendedDummy", Map(
    "part1" -> false,
    "part2" -> false,
    "part3" -> false,
  ))


}

object DummyExampleExtended {
  def apply() = new DummyExampleExtended
}
