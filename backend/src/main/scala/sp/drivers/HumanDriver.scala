package sp.drivers

import akka.actor._
import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Subscribe}
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._


/**
  * A driver for a mockup UR human. Lauch this actor and send it a
  * SetUpDeviceDriver with driverIdentifier = HumanDriver.driverType
  */
object HumanDriver {
  val driverType = "HumanDriver"
  def props = Props(classOf[HumanDriver])
}

class HumanDriver extends Actor {
  val mediator = DistributedPubSub(context.system).mediator
  mediator ! Subscribe("driverCommands", self)

  def receive = {
    case x: String =>
      println(x)
      SPMessage.fromJson(x).foreach{mess =>
        for {
          h <- mess.getHeaderAs[SPHeader]
          b <- mess.getBodyAs[APIVirtualDevice.Request]
        } yield {
          b match {
            case APIVirtualDevice.SetUpDeviceDriver(d) if d.driverType == URDriver.driverType =>
              context.actorOf(URDriverInstance.props(d), d.id.toString)
            case _ =>
          }
        }
      }
  }
}


/**
  * The actual driver instance answering the commands
  */
object HumanDriverInstance {
  def props(d: APIVirtualDevice.Driver) = Props(classOf[HumanDriverInstance], d)
}

/**
  * This is  running one UR robot, but only as a dummy
  * The driver creates a dummy UR robot, shown below
  * you can set a reference and some states
  * @param d APIVirtualDevice.Driver The name, id, and setup of the driver
  */
class HumanDriverInstance(d: APIVirtualDevice.Driver) extends Actor {
  val id = d.id
  val mediator = DistributedPubSub(context.system).mediator
  mediator ! Subscribe("driverCommands", self)

  // the state of the Human, Using a case class to simplify life
  var humanState = HumanStream(
    // position currentPos = 5 is near the robot, so the robot shouldn't move when the human is there
    refPos = 0,
    currentPos = 0,
    working = false,
  )

  // the dummy Human that we will observe with this driver
  // this will be defined based on the setup in the real system

  val myHuman = context.actorOf(DummyHuman.props(self))

  // We have started and is publishing that we exist
  // TODO: Also add messages when instance is killed.
  val header = SPHeader(from = d.name)
  val body = APIVirtualDevice.NewDriver(d)
  mediator ! Publish("driverEvents", SPMessage.makeJson(header, body))

  // All messages to the actor arrive here
  def receive = {
    case x if {println(s"id:$id, got: $x");false} => false
    // the stream from the dummy Human
    case x: HumanStream  => // matching the stream
      handleCmdDone(x)
      if (x != humanState) {
        humanState = x
        sendStateToBus(streamToMap(humanState))
      }

    case x: String =>
      // SPMessage uses the APIParser to parse the json string
      SPMessage.fromJson(x).foreach{mess =>
          for {
            h <- mess.getHeaderAs[SPHeader] // we can filter the message based on the header, but is skipping it for now
            b <- mess.getBodyAs[sp.devicehandler.APIVirtualDevice.Request] // we are expecting a case class in the Vritual device API
          } yield {
            b match {
              case sp.devicehandler.APIVirtualDevice.DriverCommand(n, driverid, state) if driverid == id  => // matching that it is a command and that it is to this driver
                handleCmd(state - "currentPos", h) // removing currentPos since that can not be set
                //mediator ! Publish("driverEvents", SPMessage.makeJson(header, body))
              case _ =>
            }
          }

      }

  }

  // Keeping track of when the variables have been written to the dummy UR
  // This can only handle one command at the time. If more is needed, this should
  // be handled with another actor, an ask request or similar.
  var reqHeader: Option[SPHeader] = None
  var changed: Map[String, SPValue] = Map()

  // Mapping from state to actual dummy UR api
  def handleCmd(state: Map[String, SPValue], h: SPHeader) = {
    // Setting up variables to check when the dummyUR is updated
    reqHeader = Some(h)
    changed = state


    for {
      x <- state.get("refPos")
      value <- x.to[Int].toOption if humanState.refPos != value
    } yield {
      myHuman ! value
    }



  }

  def handleCmdDone(upd: HumanStream) = {
    reqHeader.foreach { header =>
      val streamMap = streamToMap(upd)
      val res = changed.forall(kv => !streamMap.contains(kv._1) || streamMap.get(kv._1).contains(kv._2))
      if (res) {
        val updH = header.swapToAndFrom
        val b = sp.devicehandler.APIVirtualDevice.DriverCommandDone(updH.reqID, true) // we do not check if it fails in this case
        mediator ! Publish("driverEvents", SPMessage.makeJson(updH, b))
        reqHeader = None
        changed = Map()
      }
    }
  }

  // Sending a message to the bus
  def sendStateToBus(state: Map[String, SPValue]) = {
    val header = SPHeader(from = d.name)
    val body = sp.devicehandler.APIVirtualDevice.DriverStateChange(d.name, id, state, false)
    mediator ! Publish("driverEvents", SPMessage.makeJson(header, body))
  }



  def streamToMap(stream: HumanStream) = {
    Map[String, SPValue](
      "currentPos" -> stream.currentPos,
      "refPos" -> stream.refPos,
      "working" -> stream.working,
    )
  }




}

/**
  * A dummy human that moves around
  *
  */
class DummyHuman(replyTo: ActorRef) extends Actor {
  var cmd = "doNothing"
  var currentPos = "atHome"
  var working = false
  var cmdCompl = false

  def receive = {
    case "startWorking" => working = true
    case "stopWorking" => working = false
    case "cmd" =>
    case "moveToHome" => refPos = 20
    case x: Int => refPos = x
    case "tick" =>
      if (refPos != currentPos) {
        val diff = refPos-currentPos
        currentPos = currentPos + diff/Math.abs(diff)
      }
      // sending the state of the dummyHuman every tick
      replyTo ! makeMess
  }

  def makeMess = {
    HumanStream(currentPos, refPos, working)
  }

  // A "ticker" that sends a "tick" string to self every 0.2 second
  import context.dispatcher

  import scala.concurrent.duration._
  val ticker = context.system.scheduler.schedule(0.2 seconds, 0.2 seconds, self, "tick")
}

case class HumanStream(currentPos: Int, refPos: Int, working: Boolean)

object DummyHuman {
  def props(replyTo: ActorRef) = Props(classOf[DummyHuman], replyTo)
}
