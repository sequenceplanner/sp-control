package sp.modelSupport.ros2

import akka.actor._
import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }
import scala.concurrent.duration._

import sp.domain.Logic._
import sp.domain._

import sp.modelSupport._
import sp.virtualdevice.APISPVD._
import sp.virtualdevice.SPStreamSupport._

import sp.drivers.ros2._

trait ROSResource extends Resource {
  type DriverState = Map[String, SPValue]
  case class Sub(topic: String, messageType: String, via: Flow[DriverState,State, _])
  var subs: List[Sub] = List.empty
  case class Pub(topic: String, messageType: String, tickInterval: Option[FiniteDuration], via: Flow[State, DriverState, _])
  var pubs: List[Pub] = List.empty

  def subscribe(topic: String, messageType: String, via: Flow[DriverState, State, _]) =
    subs = Sub(topic, messageType, via) :: subs

  def publish(topic: String, messageType: String, tickInterval: Option[FiniteDuration] = None, via: Flow[State, DriverState, _]) =
    pubs = Pub(topic, messageType, tickInterval, via) :: pubs

  def makeResource(): SPResource = {
    implicit val materializer = ActorMaterializer()(system)
    val ros = new RCLBase(system)

    val initialState = things.flatMap(t => t.attributes.get("initialState").map(s=>t.id->s)).toMap

    // set up subscriber streams
    val sources = subs.map { sub =>
      ros.subscriber(sub.messageType, sub.topic).map(_.fields.toMap)
        .via(sub.via)
      // debug
      //  .map { now => println("GOT ATTR: " + now); now }
    }

    val sinks = pubs.map { pub =>
      val p = ros.publisher(pub.messageType, pub.topic)
      // start with an "empty" ros message, merge changes to field within the stream
      val empty = ROSHelpers.createROSMsg(pub.messageType).map(ROSHelpers.msgToAttr).get // getOrElse(SPAttributes()) we want to blow up on fail here
      val partialMessages = Flow[SPAttributes].scan(empty){ case (attr, partial) => attr ++ partial }
      val ticking = pub.tickInterval.map(interval => normalizeRate[SPAttributes](interval)).getOrElse(Flow[SPAttributes].map(identity))

      val toAttr = Flow[DriverState].map{ds =>
        ds.foldLeft(SPAttributes()){ case (attr, (field, spval)) => attr ++ SPAttributes(field -> spval) }
      }.filter(_.values.nonEmpty)

      pub.via.via(toAttr).via(ticking).via(partialMessages).
        // debug
        // map(a=>{println("SENDING ATTR: " +a);a}).
        to(p)
    }


    SPResource(id, initialState, sinks, sources)
  }
}
