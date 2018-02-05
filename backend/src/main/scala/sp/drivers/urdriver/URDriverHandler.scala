package sp.drivers.urdriver

import akka.actor._
import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.Subscribe
import sp.devicehandler.{APIVirtualDevice => vdapi}
import sp.domain._

object URDriverHandler {
  def props = Props(classOf[URDriverHandler])
}

class URDriverHandler extends Actor {
  val mediator = DistributedPubSub(context.system).mediator
  mediator ! Subscribe("driverCommands", self)

  val driverName = "URDriver"

  def receive = {
    case "stop" =>
      context.children.foreach { child =>
        child ! "disconnect"
      }

    case x: String =>
      println(x)
      SPMessage.fromJson(x).foreach{mess =>
          for {
            h <- mess.getHeaderAs[SPHeader]
            b <- mess.getBodyAs[vdapi.Request]
          } yield {
            b match {
              case vdapi.SetUpDeviceDriver(d) if d.driverType == driverName =>
                context.actorOf(URDriverRuntime.props(d.name, d.id, d.setup), d.id.toString)
              case _ =>
            }
          }
      }
    case _ => sender ! APISP.SPError("Ill formed request")
  }
}


