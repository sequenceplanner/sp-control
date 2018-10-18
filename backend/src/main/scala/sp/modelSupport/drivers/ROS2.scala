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

import sp.drivers.ros2._

trait ROSResource extends Resource {
  case class Sub(topic: String, messageType: String, mapping: Map[String, ID])
  var subs: List[Sub] = List.empty
  case class Pub(topic: String, messageType: String, tickInterval: Option[FiniteDuration], mapping: Map[ID, String])
  var pubs: List[Pub] = List.empty

  def subscribe(topic: String, messageType: String, mapping: Map[String, ID] = Map()) =
    subs = Sub(topic, messageType, mapping) :: subs

  def publish(topic: String, messageType: String, tickInterval: Option[FiniteDuration] = None, mapping: Map[ID, String] = Map()) =
    pubs = Pub(topic, messageType, tickInterval, mapping) :: pubs


  def makeResource(system: ActorSystem): SPResource = {
    implicit val materializer = ActorMaterializer()(system)
    val ros = new RCLBase(system)


    val initialState = things.flatMap(t => t.attributes.get("initialState").map(s=>t.id->s)).toMap

    // set up subscriber streams
    val sources = subs.map { sub =>
      ros.subscriber(sub.messageType, sub.topic)
        .map(attr => sub.mapping.flatMap { case (fieldname, id) => attr.get(fieldname).map(spval => id -> spval) }.toMap)
      // debug
        .scan(State.empty){case (last, now) => if (now.toSet.diff(last.toSet).nonEmpty) {println("GOT ATTR: " + now); now } else now}
    }

    val sinks = pubs.map { pub =>
      val p = ros.publisher(pub.messageType, pub.topic)
      // start with an "empty" ros message, merge changes to field within the stream
      val empty = ROSHelpers.createROSMsg(pub.messageType).map(ROSHelpers.msgToAttr).get // getOrElse(SPAttributes()) we want to blow up on fail here
      val partialMessages = Flow[SPAttributes].scan(empty){ case (attr, partial) => attr ++ partial }
      val ticking = pub.tickInterval.map(interval => Flow[SPAttributes].merge(Source.tick(interval, interval, SPAttributes()))).getOrElse(Flow[SPAttributes].map(identity))

      val toMsg = Flow[State].map{state =>
        val attrFields = state.flatMap { case (id, spval) =>
          pub.mapping.get(id).map(fieldname => fieldname -> spval)
        }
        attrFields.foldLeft(SPAttributes()){ case (attr, (field, spval)) => attr ++ SPAttributes(field -> spval) }
      }.filter(_.values.nonEmpty)
      // debug
        .map(a=>{println("SENDING ATTR: " +a);a})

      toMsg.via(ticking).via(partialMessages).to(p)
    }


    SPResource(id, initialState, sinks, sources)
  }
}
