package sp.unification

import sp.domain.Logic._
import sp.domain._
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
  v("refPos", 0, List(0))
  v("currentPos", 0, List(0))

  a("moveToPos", List("refPos"),
    c("pre", "true", "active := true"),
    c("started", "currentPos != refPos"),
    c("post", "currentPos == refPos"),
    c("reset", "true"))


  driver(dn, URDriver.driverType)
  resource(rn) // blank list of things = take everything
}

class AHuman(name: String) extends ModelDSL {

  val dn=name + "driver"
  val rn= name + "resource"

  dv("humanName", dn,"humanName")
  dv("humanID", dn,"humanID")
  dv("loggedIn", dn,"loggedIn")
  dv("cmd", dn,"cmd")
  dv("ack", dn,"ack")
  dv("done", dn,"done")

  val instructions = Map(
    "doSomeStuff" -> "You should do some stuff",
    "doSomethingElse" -> "Please do something else"
  )

  instructions.keys.foreach { an =>
    a(an, List(),
      c("pre", "true", s"cmd := $an"),
      c("started", "ack || done"),  // maybe change to cmd != ''
      c("post", "done", "cmd := ''", "ack := false", "done := false"),
      c("reset", "true")
    )
  }

  driver(dn, sp.drivers.HumanDriver.driverType, SPAttributes("instructions" -> instructions))
  resource(rn) // blank list of things = take everything
}

class DummyExampleExtended extends ModelDSL {
  use("R1", new DummyRobot("1"))
  use("R2", new DummyRobot("2"))
  use("R3", new DummyRobot("3"))
//  use("karen", new AHuman("karen"))

  v("part1", false, List(false, true))
  v("part2", false, List(false, true))
  v("part3", false, List(false, true))

  o("R1_place1", "R1.moveToPos")(
    c("pre", "!part1 && !part3 && R1.refPos = 0", "R1.refPos := 50"),
    c("post", "true", "part1 := true"),
    c("reset", "true")
  )

  o("R1_remove1", "R1.moveToPos")(
    c("pre", "part1 && part3 && R1.refPos = 50", "R1.refPos := 0"),
    c("post", "true", "part1 := false"),
    c("reset", "true")
  )

  o("R2_place2", "R2.moveToPos")(
    c("pre", "!part2 && !part3", "R2.refPos = 75"),
    c("post", "true", "part2 := true"),
    c("reset", "true")
  )

  o("R2_remove2", "R2.moveToPos")(
    c("pre", "part2 && part3", "R2.refPos = 10"),
    c("post", "true", "part2 := false"),
    c("reset", "true")
  )

  o("R3_place3", "R3.moveToPos")(
    c("pre", "!part3 && part1 && part2", "R3.refPos = 50"),
    c("post", "true", "part3 := true"),
    c("reset", "true")
  )

  o("R3_remove3", "R3.moveToPos")(
    c("pre", "part3 && !part1 && !part2", "R3.refPos = 0"),
    c("post", "true", "part3 := false"),
    c("reset", "true")
  )

  runner("extendedDummyRunner")

  // def addSop(idables: List[IDAble]) = {
  //   val opMap = idables.collect { case o: Operation if o.attributes.getAs[String]("isa")!=Some("Ability") => (o.name, o.id) }.toMap

  //   println("OP MAP: ")
  //   println(opMap.mkString("\n"))

  //   val r1place1 = opMap("R1_place1")
  //   val r2place2 = opMap("R2_place2")
  //   val r3place3 = opMap("R3_place3")
  //   val r1remove1 = opMap("R1_remove1")
  //   val r2remove2 = opMap("R2_remove2")
  //   val r3remove3 = opMap("R3_remove3")

  //   val sop = SOPSpec("Main sequence", List(
  //     Sequence(List(
  //       Parallel(List(SOP(r1place1), SOP(r2place2))),
  //       SOP(r3place3),
  //       Parallel(List(SOP(r1remove1), SOP(r2remove2))),
  //       SOP(r3remove3)))))

  //   sop :: idables
  // }
  // addPostBuildHook(addSop)
}

object DummyExampleExtended {
  def apply() = new DummyExampleExtended
}
