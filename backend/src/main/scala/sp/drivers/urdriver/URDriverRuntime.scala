package sp.drivers.urdriver

import java.util.UUID
import java.util.concurrent.TimeUnit

import akka.actor._
import akka.cluster.pubsub.DistributedPubSub
import akka.cluster.pubsub.DistributedPubSubMediator.{Publish, Subscribe}
import sp.domain.Logic._
import sp.domain._



object URDriverRuntime {
  def props(name: String, id: UUID, setup: SPAttributes) = Props(classOf[URDriverRuntime], name, id, setup)
}


/**
  * This is  running one UR robot, but only as a dummy
  * The driver creates a dummy UR robot, shown below
  * you can set a reference and some states
  * @param name The name of the driver
  * @param id The id of the driver
  * @param setup Currently there is no setup requiered since it is a dummy, but soon it will need it!
  */
class URDriverRuntime(name: String, id: UUID, setup: SPAttributes) extends Actor {
  import context.dispatcher
  val mediator = DistributedPubSub(context.system).mediator
  mediator ! Subscribe("driverCommands", self)

  // the internal state of the UR-robot, Using a case class to simplify life
  var urState = URStream(
    currentPos = 0,
    refPos = 0,
    active = false,
    hasTool = false
  )

  // the dummy UR that we will talk to with this driver
  // this will be defined based on the setup in the real system
  // in this case the dummy UR will send messages to us since we
  // are its parents in the actor hierarchy
  val myUR = context.actorOf(DummyUR.props)

  // All messages to the actor arrive here
  def receive = {
    // the stream from the dummy UR
    case x: URStream  => // matching the stream
      handleCmdDone(x)
      if (x != urState) {
        urState = x
        sendStateToBus(streamToMap(urState))
      }

    // The driver handler want to terminate the driver
    case "disconnect" =>
      // clean up the connection with the real system
      myUR ! PoisonPill
      self ! PoisonPill

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

    // Converting to the actual api
    for {
      x <- state.get("active")
      value <- x.to[Boolean] if urState.active != value
    } yield {
      val send = if (value) "activate" else "deactivate"
      myUR ! send
    }

    for {
      x <- state.get("hasTool")
      value <- x.to[Boolean] if urState.hasTool != value
    } yield {
      val send = if (value) "addTool" else "removeTool"
      myUR ! send
    }

    for {
      x <- state.get("refPos")
      value <- x.to[Int] if urState.refPos != value
    } yield {
      myUR ! value
    }



  }

  def handleCmdDone(upd: URStream) = {
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
    val header = SPHeader(from = name)
    val body = sp.devicehandler.APIVirtualDevice.DriverStateChange(name, id, state, false)
    mediator ! Publish("driverEvents", SPMessage.makeJson(header, body))
  }



  def streamToMap(stream: URStream) = {
    Map[String, SPValue](
      "currentPos" -> stream.currentPos,
      "refPos" -> stream.refPos,
      "active" -> stream.active,
      "hasTool" -> stream.hasTool
    )
  }




}

/**
  * A dummy UR robot that has one joint that it will update based on the refPos
  * Will only move if it is in the active state
  */
class DummyUR extends Actor {
  var currentPos = 0
  var refPos = 0
  var active = false
  var hasTool = false

  def receive = {
    case "activate" => active = true
    case "deactivate" => active = false
    case "addTool" => hasTool = true
    case "removeTool" => hasTool = false
    case x: Int => refPos = x
    case "resetMove" => refPos = currentPos
    case "tick" =>
      // only move the robot when active
      if (active) {
        val diff = refPos-currentPos
        currentPos = currentPos + diff/Math.abs(diff)
      }
      // sending the state of the dummyUR every tick
      context.parent ! makeMess
  }

  def makeMess = {
    URStream(currentPos, refPos, active, hasTool)
  }

  // A "ticker" that sends a "tick" string to self every 0.2 second
  import scala.concurrent.duration._
  import context.dispatcher
  val ticker = context.system.scheduler.schedule(0.2 seconds, 0.2 seconds, self, "tick")
}

case class URStream(currentPos: Int, refPos: Int, active: Boolean, hasTool: Boolean)

object DummyUR {
  def props = Props(classOf[DummyUR])
}
