package sp.unification

import sp.domain.Logic._
import sp.domain._
import sp.drivers.URDriver
import sp.modelSupport._


class DummyExampleWithSOP extends ModelDSL {
  use("R1", new DummyRobot("1"))
  use("R2", new DummyRobot("2"))
  use("R3", new DummyRobot("3"))
  use("OP", new AHuman("OP"))
  use("TON", new TON("TON"))


  val r1 = List("r1")
  val r2 = List("r2")
  val r3 = List("r3")
  val z1 = List("z1")
  val z2 = List("z2")



  o("R1_place1", "R1.moveToPos", r1 ++ z1)(
    c("pre", "true", "R1.refPos := 10"),
  )
  o("R1_remove1", "R1.moveToPos", r1)(
    c("pre", "true", "R1.refPos := 0"),
  )
  o("R2_place2", "R2.moveToPos", r2 ++ z1)(
    c("pre", "true", "R2.refPos := 20"),
  )
  o("R2_remove2", "R2.moveToPos", r2)(
    c("pre", "true", "R2.refPos := 0"),
  )
  o("R3_place3", "R3.moveToPos", r3)(
    c("pre", "true", "R3.refPos := 50"),
  )
  o("R3_remove3", "R3.moveToPos", r3)(
    c("pre", "true", "R3.refPos := 60"),
  )
  o("DoSome", "OP.doSomeStuff")()
  o("DoElse", "OP.doSomethingElse")()


  // testing SOP for simple sequences



  sop("test")(
    c("pre", "true")
  )(
    sO("DoSome"),
    sS(
      sO("R1_place1"),
      sOnew(s"delayBefore2", s"TON.delay", List("TON"))(cond("pre", "true", "TON.pt := 2000")),
      sO("R2_place2")
    ),
    sO("R3_place3"),
    sO("DoElse"),
    sA(
      sO("R1_remove1"),
      sO("R2_remove2")
    ),
    sO("R3_remove3"),
  )


  runner("extendedDummyRunner")

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

object DummyExampleWithSOP {
  def apply() = new DummyExampleWithSOP
}
