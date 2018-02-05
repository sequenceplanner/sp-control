package sp.urdriver

import java.util.UUID
import java.util.concurrent.TimeUnit

import akka.actor._
import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Subscribe}
import sp.domain.Logic._
import sp.domain._
import sp.milowrapper.{MiloOPCUAClient, StateUpdate}

import scala.concurrent.duration._
// the vd api
import sp.devicehandler.{APIVirtualDevice => vdapi}


object URDriverRuntime {
  def props(name: String, id: UUID, setup: SPAttributes) = Props(classOf[URDriverRuntime], name, id, setup)
}

class URDriverRuntime(name: String, id: UUID, setup: SPAttributes) extends Actor {
  import context.dispatcher
  val mediator = DistributedPubSub(context.system).mediator
  mediator ! Subscribe("driverCommands", self)

  self ! "connect"

  def receive = {
    case "connect" =>


    case "disconnect" =>

    case x: String =>
      // SPMessage uses the APIParser to parse the json string
      SPMessage.fromJson(x).foreach{mess =>
          for {
            h <- mess.getHeaderAs[SPHeader]
            b <- mess.getBodyAs[vdapi.Request]
          } yield {
            b match {
              case vdapi.DriverCommand(name, driverid, state)  =>

                //mediator ! Publish("driverEvents", SPMessage.makeJson(header, body))
              case _ =>
            }
          }

      }
    case StateUpdate(activeState) =>
//      val header = SPHeader(from = name)
//      val stateWithTime = activeState + ("timestamp" -> SPValue(g))
//      val body = vdapi.DriverStateChange(name, id, stateWithTime, false)
//      mediator ! Publish("driverEvents", SPMessage.makeJson(header, body))
    case _ => sender ! APISP.SPError("Ill formed request")
  }

}
