package sp.unification

import scala.concurrent.duration._

import sp.domain.Logic._
import sp.domain._

import sp.modelSupport._
import sp.modelSupport.ros2._

object ModelData {
  val placePos = Map("1" -> SPValue(50), "2" -> SPValue(75), "3" -> SPValue(50))
  val removePos = Map("1" -> SPValue(0), "2" -> SPValue(10), "3" -> SPValue(0))
}

class Robot(name: String) extends ROSResource {
  val subMapping = stringToIDMapper(Map("act_pos" -> vu("actPos", ModelData.removePos(name), List(ModelData.removePos(name),ModelData.placePos(name)))))
  subscribe(s"extended_dummy$name/state", "unification_msgs.msg.State", subMapping)

  val pubMapping = IDToStringMapper(Map(v("refPos", ModelData.removePos(name), List(ModelData.removePos(name),ModelData.placePos(name))) -> "ref_pos",
    v("active", SPValue(false), List(SPValue(false), SPValue(true))) -> "active"))

  publish(s"extended_dummy$name/control", "unification_msgs.msg.Control", Some(1000.millis), pubMapping)

  // synchronous runner allows this now
  v("booked", false)

  a("moveToPos")(
    c("pre", "!booked", "booked := true", "active := true"),
    c("started", "actPos != refPos"),
    c("post", "actPos == refPos", "booked := false"),
    c("reset", "true"))

}

class NewExtended extends MiniModel {
  use("R1", new Robot("1"))
  use("R2", new Robot("2"))
  use("R3", new Robot("3"))

  v("part1", false)
  v("part2", false)
  v("part3", false)

  val r1place1 = o("R1_place1", "R1.moveToPos")(
    c("pre", "!part1 && R1.refPos = 0", "R1.refPos := 50"),
    c("post", "true", "part1 := true"),
    c("reset", "true")
  )

  val r1remove1 = o("R1_remove1", "R1.moveToPos")(
    c("pre", "part1 && part3 && R1.refPos = 50", "R1.refPos := 0"),
    c("post", "true", "part1 := false"),
    c("reset", "true")
  )

  val r2place2 = o("R2_place2", "R2.moveToPos")(
    c("pre", "!part2 && !part3", "R2.refPos := 75"),
    c("post", "true", "part2 := true"),
    c("reset", "true")
  )

  val r2remove2 = o("R2_remove2", "R2.moveToPos")(
    c("pre", "part2 && part3", "R2.refPos := 10"),
    c("post", "true", "part2 := false"),
    c("reset", "true")
  )

  val r3place3 = o("R3_place3", "R3.moveToPos")(
    c("pre", "!part3 && part1 && part2", "R3.refPos := 50"),
    c("post", "true", "part3 := true"),
    c("reset", "true")
  )

  val r3remove3 = o("R3_remove3", "R3.moveToPos")(
    c("pre", "part3 && !part1 && !part2", "R3.refPos := 0"),
    c("post", "true", "part3 := false"),
    c("reset", "true")
  )

  // test synthesis and guard extraction
  x("nogo", List("R1_place1_exec && R2_place2_exec"))
  x("nogo2", List("R2_remove2_exec && R1_remove1_exec"))


  sop("Main sequence", List(
    Sequence(List(
      Parallel(List(SOP(r1place1), SOP(r2place2))),
      SOP(r3place3),
      Parallel(List(SOP(r1remove1), SOP(r2remove2))),
      SOP(r3remove3)))))

}

object NewExtended {
  def apply() = new NewExtended
}
