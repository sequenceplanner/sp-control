package sp.unification

import sp.domain.Logic._
import sp.drivers.URDriver
import sp.modelSupport._

class DummyRobot(n: String) extends ModelDSL {
  val dn="driver"+n
  val rn="resource"+n

  dv("currentPos", dn,"currentPos")
  dv("hasTool", dn, "hasTool")

  // cmd
  dv("active", dn, "active")
  dv("refPos", dn, "refPos")

  // Klurigt med integer som domän...
  v("refPos", 0, List())
  v("currentPos", 0, List())

  a("moveToPos", List("refPos"),
    c("pre", "true", "active := true"),
    c("started", "currentPos != refPos"),
    c("post", "currentPos == refPos"),
    c("reset", "true"))


  driver(dn, URDriver.driverType)
  resource(rn) // blank list of things = take everything
}

class DummyExampleExtended extends ModelDSL {
  use("R1", new DummyRobot("1"))
  use("R2", new DummyRobot("2"))
  use("R3", new DummyRobot("3"))

  v("part1", false, List(false, true))
  v("part2", false, List(false, true))
  v("part3", false, List(false, true))

  o("R1_place1", "R1.moveToPos")(
    c("pre", "!part1 && !part3", "R1.refPos = 100"),
    c("post", "false", "part1 = true"),
    c("reset", "true")
  )

  o("R1_remove1", "R1.moveToPos")(
    c("pre", "part1 && part3", "R1.refPos = 10"),
    c("post", "false", "part1 = false"),
    c("reset", "true")
  )

  o("R2_place2", "R2.moveToPos")(
    c("pre", "!part2 && !part3", "R2.refPos = 75"),
    c("post", "false", "part2 = true"),
    c("reset", "true")
  )

  o("R2_remove1", "R2.moveToPos")(
    c("pre", "part2 && part3", "R2.refPos = 10"),
    c("post", "false", "part2 = false"),
    c("reset", "true")
  )

  o("R3_place3", "R3.moveToPos")(
    c("pre", "!part3 && part1 && part2", "R3.refPos = 50"),
    c("post", "false", "part3 = true"),
    c("reset", "true")
  )

  o("R3_remove3", "R3.moveToPos")(
    c("pre", "part3 && !part1 && !part2", "R3.refPos = 0"),
    c("post", "false", "part3 = false"),
    c("reset", "true")
  )

  runner("extendedDummyRunner")


}

object DummyExampleExtended {
  def apply() = new DummyExampleExtended
}
