package sp.unification

import scala.concurrent.duration._

import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }

import sp.domain.Logic._
import sp.domain._

import sp.modelSupport._
import sp.modelSupport.ros2._

object ModelData {
  val R1poseNames = Map("place" -> 50, "remove" -> 0).map { case (k,v) => (SPValue(k)->SPValue(v)) }
  val R2poseNames = Map("place" -> 75, "remove" -> 10).map { case (k,v) => (SPValue(k)->SPValue(v)) }
  val R3poseNames = Map("place" -> 50, "remove" -> 0).map { case (k,v) => (SPValue(k)->SPValue(v)) }

  val data = Map(
    "1" -> (R1poseNames, R1poseNames.map(_.swap)),
    "2" -> (R2poseNames, R2poseNames.map(_.swap)),
    "3" -> (R3poseNames, R3poseNames.map(_.swap)),
  )

  def apply(s: String) = data(s)
}

class Robot(name: String) extends ROSResource {
  val actPos = vu("actPos", "remove", List("remove", "place", "moving"))
  val hasTool = vu("hasTool", false)

  val subMapping = stringToIDMapper(Map(
    "act_pos" -> actPos,
    "has_tool" -> hasTool
  ))

  val toDomainMapping = mapDomain(Map(
    actPos -> (spv => ModelData(name)._2.get(spv).getOrElse(SPValue("moving")))))

  val subFlow = subMapping.via(toDomainMapping)
  subscribe(s"extended_dummy$name/state", "unification_msgs.msg.State", subFlow)

  val refPos = v("refPos", s"remove", List("remove", "place"))
  val active = v("active", false)
  val pubMapping = IDToStringMapper(Map(refPos -> "ref_pos", active -> "active"))

  val fromDomainMapping = mapDomain(Map(
    refPos -> (spv => ModelData(name)._1.get(spv).getOrElse(SPValue(-1)))))

  val pubFlow = fromDomainMapping.via(pubMapping)

  publish(s"extended_dummy$name/control", "unification_msgs.msg.Control", Some(1000.millis), pubFlow)

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

  vm("part1", false, Set(false))   // liveness for parts via markings
  vm("part2", false, Set(false))
  vm("part3", false, Set(false))

  val r1place1 = o("R1_place1", "R1.moveToPos")(
    c("pre", "!part1 && !part3", "R1.refPos := 'place'"),
    c("post", "true", "part1 := true"),
    c("reset", "true")
  )

  val r1remove1 = o("R1_remove1", "R1.moveToPos")(
    c("pre", "part1 && part3", "R1.refPos := 'remove'"),
    c("post", "true", "part1 := false"),
    c("reset", "true")
  )

  val r2place2 = o("R2_place2", "R2.moveToPos")(
    c("pre", "!part2 && !part3", "R2.refPos := 'place'"),
    c("post", "true", "part2 := true"),
    c("reset", "true")
  )

  val r2remove2 = o("R2_remove2", "R2.moveToPos")(
    c("pre", "part2 && part3", "R2.refPos := 'remove'"),
    c("post", "true", "part2 := false"),
    c("reset", "true")
  )

  val r3place3 = o("R3_place3", "R3.moveToPos")(
    c("pre", "!part3 && part1 && part2", "R3.refPos := 'place'"),
    c("post", "true", "part3 := true"),
    c("reset", "true")
  )

  val r3remove3 = o("R3_remove3", "R3.moveToPos")(
    c("pre", "part3 && !part1 && !part2", "R3.refPos := 'remove'"),
    c("post", "true", "part3 := false"),
    c("reset", "true")
  )

  // test synthesis and guard extraction
  x("nogo", List("R1_place1 == 'executing' && R2_place2 == 'executing'"))
  x("nogo", List("R2_place2 == 'executing' && R3_place3 == 'executing'"))

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
