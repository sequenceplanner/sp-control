package sp.modelSupport.opcua

import akka.actor._
import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }
import scala.concurrent.duration._

import sp.domain.Logic._
import sp.domain._

import sp.modelSupport._
import sp.virtualdevice.APISPVD._

import sp.milowrapper._

trait OPCUAResource extends Resource {
  type DriverState = Map[String, SPValue]
  val url: String

  case class Sub(identifiers: List[String], samplingInterval: Double, via: Flow[DriverState,State, _])
  var subs: List[Sub] = List.empty
  case class Pub(identifiers: List[String], tickInterval: Option[FiniteDuration], via: Flow[State, DriverState, _])
  var pubs: List[Pub] = List.empty

  def subscribe(identifiers: List[String], samplingInterval: Double, via: Flow[DriverState, State, _]) =
    subs = Sub(identifiers, samplingInterval, via) :: subs

  def publish(identifiers: List[String], tickInterval: Option[FiniteDuration] = None, via: Flow[State, DriverState, _]) =
    pubs = Pub(identifiers, tickInterval, via) :: pubs

  def makeResource(): SPResource = {
    implicit val materializer = ActorMaterializer()(system)

    val client = new MiloOPCUAClient()
    client.connect(url)

    val initialState = things.flatMap(t => t.attributes.get("initialState").map(s=>t.id->s)).toMap

    val sources = subs.map(s => {
      Source.actorRef[StateUpdate](10, OverflowStrategy.dropHead)
        .mapMaterializedValue { ref =>
          client.subscribeToNodes(s.identifiers, ref, s.samplingInterval)
          ref
        }
        .map(su => {println("GOT OPC STATE!: " + su.activeState); su.activeState})
    }.via(s.via))

    val sinks = pubs.map { pub =>
      val ticking = pub.tickInterval.map(interval => Flow[DriverState].merge(Source.tick(interval, interval, Map[String, SPValue]()))
        .scan(Map[String, SPValue]()){ case (acum, ns) => acum ++ ns }).getOrElse(Flow[DriverState].map(identity))
      val sink = Sink.foreach[DriverState] { ds =>
        ds.foreach { case (ident, spval) => client.write(ident, spval) }
      }

      pub.via.to(sink)
    }


    SPResource(id, initialState, sinks, sources)
  }
}
