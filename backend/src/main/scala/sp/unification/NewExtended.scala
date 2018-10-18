package sp.unification

import scala.concurrent.duration._

import sp.domain.Logic._
import sp.domain._

import sp.modelSupport._
import sp.modelSupport.ros2._

class Robot(name: String) extends ROSResource {
  subscribe(s"extended_dummy$name/state", "unification_msgs.msg.State",
    Map("act_pos" -> v("actPos")))

  publish(s"extended_dummy$name/control", "unification_msgs.msg.Control", Some(1000.millis), Map(
      v("refPos", 0) -> "ref_pos",
      v("active", false) -> "active"))

  // synchronous runner allows this now
  v("booked", false)

  a("moveToPos")(
    c("pre", "!booked", "booked := true", "active := true"),
    c("started", "actPos != refPos"),
    c("post", "actPos == refPos"),
    c("reset", "true", "booked := false"))
}

class NewExtended extends MiniModel {
  use("R1", new Robot("1"))
  use("R2", new Robot("2"))
  use("R3", new Robot("3"))

  v("part1", false)
  v("part2", false)
  v("part3", false)

  val r1place1 = o("R1_place1", "R1.moveToPos")(
    c("pre", "!part1 && !part3 && R1.refPos = 0", "R1.refPos := 50"),
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
