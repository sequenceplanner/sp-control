package sp.sdu

import scala.concurrent.duration._

import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }

import akka.actor._

import sp.domain.Logic._
import sp.domain._

import sp.modelSupport._
import sp.modelSupport.opcua._

object UR {
  val initialState = "Unknown"

  val poses = List("Unknown", "above_station",
    "above_blue", "at_blue", "above_place_blue", "place_blue",
    "above_red", "at_red", "above_place_red", "place_red",
    "above_yellow", "at_yellow", "above_place_yellow", "place_yellow",
    "above_green", "at_green", "above_place_green", "place_green"
  )
}


class UR(override val system: ActorSystem) extends OPCUAResource {
  val url = "opc.tcp://localhost:12686"

  val booked = v("booked", false) // resource booking

  val subIdents = List("B941WeldSeg1_end")
  val B941WeldSeg1_end = i("B941WeldSeg1_end", false)
  val subMapping = stringToIDMapper(Map("B941WeldSeg1_end" -> B941WeldSeg1_end))

  subscribe(subIdents, 100, subMapping)


  val pubIdents = List("B941WeldSeg1_start")
  val B941WeldSeg1_start = o("B941WeldSeg1_start", false)
  val pubMapping = IDToStringMapper(Map(B941WeldSeg1_start -> "B941WeldSeg1_start"))

  publish(pubIdents, Some(1000.millis), pubMapping)

  a("weldSeg1")(
    c("pre", "!booked && !B941WeldSeg1_start && !B941WeldSeg1_end", "booked := true", "B941WeldSeg1_start := true"),
    c("started", "B941WeldSeg1_start && B941WeldSeg1_end == false"),
    c("post", "B941WeldSeg1_end", "booked := false", "B941WeldSeg1_start := false"),
    c("reset", "true"))
}

class Model(override val system: ActorSystem) extends MiniModel {
  use("ur", new UR(system))
  v("done", true)

  val weldSeg1 = o("ur.weldSeg1", "ur.weldSeg1")(
    c("pre", "true"),
    c("post", "true"),
    c("reset", "true")
  )

  // synthesis
  synthesize()

}
