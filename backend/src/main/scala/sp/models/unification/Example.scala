package sp.models.unification.example

import scala.concurrent.duration._

import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }

import akka.actor._

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

class Robot(name: String, override val system: ActorSystem) extends ROSResource {
  val actPos = i("actPos", "remove", List("remove", "place", "moving"))
  val hasTool = i("hasTool", false)

  val subMapping = stringToIDMapper(Map(
    "act_pos" -> actPos,
    "has_tool" -> hasTool
  ))

  val toDomainMapping = mapDomain(Map(
    actPos -> (spv => ModelData(name)._2.get(spv).getOrElse(SPValue("moving")))))

  val subFlow = subMapping.via(toDomainMapping)
  subscribe(s"extended_dummy$name/state", "extended_dummy_messages/State", subFlow)

  val refPos = o("refPos", s"remove", List("remove", "place"))
  val active = o("active", false)
  val pubMapping = IDToStringMapper(Map(refPos -> "ref_pos", active -> "active"))

  val fromDomainMapping = mapDomain(Map(
    refPos -> (spv => ModelData(name)._1.get(spv).getOrElse(SPValue(-1)))))

  val pubFlow = fromDomainMapping.via(pubMapping)

  publish(s"extended_dummy$name/control", "extended_dummy_messages/Control", Some(1000.millis), pubFlow)

  a("moveToPos")(
    c("pre", "!active", "active := true"),
    c("isExecuting", "actPos != refPos"),
    c("executingEffect", "true", "actPos := refPos"),
    c("isFinished", "actPos == refPos", "active := false"),
    c("reset", "true"))

}

class Example(override val system: ActorSystem) extends MiniModel {
  use("R1", new Robot("1", system))
  use("R2", new Robot("2", system))
  use("R3", new Robot("3", system))

  // global estimated state
  v("part1", false)
  v("part2", false)
  v("part3", false)

  // instantiation of abilities into global system model
  val r1place1 = o("R1_place1", "R1.moveToPos", "r1")(
    c("pre", "!part1 && !part3 && R1.actPos != 'place'", "R1.refPos := 'place'"),
    c("isFinished", "true", "part1 := true"),
  )

  val r1remove1 = o("R1_remove1", "R1.moveToPos", "r1")(
    c("pre", "part1 && part3", "R1.refPos := 'remove'"),
    c("isFinished", "true", "part1 := false"),
  )

  val r2place2 = o("R2_place2", "R2.moveToPos", "r2")(
    c("pre", "!part2 && !part3", "R2.refPos := 'place'"),
    c("isFinished", "true", "part2 := true"),
  )

  val r2remove2 = o("R2_remove2", "R2.moveToPos", "r2")(
    c("pre", "part2 && part3", "R2.refPos := 'remove'"),
    c("isFinished", "true", "part2 := false"),
  )

  val r3place3 = o("R3_place3", "R3.moveToPos", "r3")(
    c("pre", "!part3 && part1 && part2", "R3.refPos := 'place'"),
    c("isFinished", "true", "part3 := true"),
  )

  val r3remove3 = o("R3_remove3", "R3.moveToPos", "r3")(
    c("pre", "part3 && !part1 && !part2", "R3.refPos := 'remove'"),
    c("isFinished", "true", "part3 := false"),
  )

  val placePart3 = o("placePart3", attr = SPAttributes("notInModel" -> true, "hasGoal" -> true, "isa" -> "operation"))(
    c("pre", s"!part3"),
    c("post", s"part3")
  )

  val removePart3 = o("removePart3", attr = SPAttributes("notInModel" -> true, "hasGoal" -> true, "isa" -> "operation"))(
    c("pre", s"part3"),
    c("post", s"!part3")
  )

  // instantiate abilities
  println("make ops")
  makeOps()

  val highLevelOps = operations.filter(_.attributes.getAs[String]("isa").contains("operation"))
  val abilities = operations.filterNot(_.attributes.getAs[String]("isa").contains("operation"))
  // just build a simple sop to visualize the ability states
  // make a grid with four columns to utilize the space we have in the widget
  val grid = List(0,1,2,3).map(n=>abilities.sliding(4,4).flatMap(_.lift(n)).toList)
  sop("abilities", List(Parallel(grid.map(s=>Sequence(s.map(o=>SOP(o)))))))
  sop("operations", List(Sequence(highLevelOps.map(o=>SOP(o)))))

  exportNuXmv("example.smv")
}
