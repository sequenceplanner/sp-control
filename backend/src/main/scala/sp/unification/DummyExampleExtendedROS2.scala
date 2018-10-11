package sp.unification

import sp.domain.Logic._
import sp.domain._
import sp.drivers.ros2._
import sp.modelSupport._

class DummyRobotROS2(n: String) extends ModelDSL with ROS2ModelSupport {
  val dn="driver"+n
  val rn="resource"+n

  // state
  subscribe(dn, "unification_msgs.msg.State", s"extended_dummy$n/state")

  // cmd
  publish(dn, "unification_msgs.msg.Control", s"extended_dummy$n/control", 1000)

  a("moveToPos", List("ref_pos"),
    c("pre", "true", "active := true"),
    c("started", "act_pos != ref_pos"),
    c("post", "act_pos == ref_pos"),
    c("reset", "true"))


  driver(dn, ROS2FlatStateDriver.driverType)
  resource(rn) // blank list of things = take everything
}

class DummyExampleExtendedROS2 extends ModelDSL {
  use("R1", new DummyRobotROS2("1"))
  use("R2", new DummyRobotROS2("2"))
  use("R3", new DummyRobotROS2("3"))

  v("part1", false, List(false, true))
  v("part2", false, List(false, true))
  v("part3", false, List(false, true))

  o("R1_place1", "R1.moveToPos")(
    c("pre", "!part1 && !part3 && R1.ref_pos = 0", "R1.ref_pos := 50"),
    c("post", "true", "part1 := true"),
    c("reset", "true")
  )

  o("R1_remove1", "R1.moveToPos")(
    c("pre", "part1 && part3 && R1.ref_pos = 50", "R1.ref_pos := 0"),
    c("post", "true", "part1 := false"),
    c("reset", "true")
  )

  o("R2_place2", "R2.moveToPos")(
    c("pre", "!part2 && !part3", "R2.ref_pos = 75"),
    c("post", "true", "part2 := true"),
    c("reset", "true")
  )

  o("R2_remove2", "R2.moveToPos")(
    c("pre", "part2 && part3", "R2.ref_pos = 10"),
    c("post", "true", "part2 := false"),
    c("reset", "true")
  )

  o("R3_place3", "R3.moveToPos")(
    c("pre", "!part3 && part1 && part2", "R3.ref_pos = 50"),
    c("post", "true", "part3 := true"),
    c("reset", "true")
  )

  o("R3_remove3", "R3.moveToPos")(
    c("pre", "part3 && !part1 && !part2", "R3.ref_pos = 0"),
    c("post", "true", "part3 := false"),
    c("reset", "true")
  )

  runner("extendedDummyROS2Runner")

  def addSop(idables: List[IDAble]) = {
    val opMap = idables.collect { case o: Operation if o.attributes.getAs[String]("isa")!=Some("Ability") => (o.name, o.id) }.toMap

    println("OP MAP: ")
    println(opMap.mkString("\n"))

    val r1place1 = opMap("R1_place1")
    val r2place2 = opMap("R2_place2")
    val r3place3 = opMap("R3_place3")
    val r1remove1 = opMap("R1_remove1")
    val r2remove2 = opMap("R2_remove2")
    val r3remove3 = opMap("R3_remove3")

    val sop = SOPSpec("Main sequence", List(
      Sequence(List(
        Parallel(List(SOP(r1place1), SOP(r2place2))),
        SOP(r3place3),
        Parallel(List(SOP(r1remove1), SOP(r2remove2))),
        SOP(r3remove3)))))

    sop :: idables
  }
  addPostBuildHook(addSop)
}

object DummyExampleExtendedROS2 {
  def apply() = new DummyExampleExtendedROS2
}
