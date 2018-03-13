package sp.drivers

import akka.actor._
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._

import sp.devicehandler.{APIDeviceDriver => api}



/**
  * A driver for a mockup UR robot. Launch this actor and send it a
  * SetUpDeviceDriver with driverIdentifier = URDriver.driverType
  */
object URDriver {
  val driverType = "URDriver"
  def props = DriverBase.props(driverType, URDriverInstance.props)
}



/**
  * The actual driver instance answering the commands
  */
object URDriverInstance {
  def props(d: VD.Driver) = Props(classOf[URDriverInstance], d)
}

/**
  * This is  running one UR robot, but only as a dummy
  * The driver creates a dummy UR robot, shown below
  * you can set a reference and some states
  * @param d APIVirtualDevice.Driver The name, id, and setup of the driver
  */
class URDriverInstance(d: VD.Driver) extends Actor
  with ActorLogging
  with sp.service.MessageBussSupport {


  subscribe(api.topicRequest)


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
  val myUR = context.actorOf(DummyUR.props(self))

  // We have started and is publishing that we exist
  // TODO: Also add messages when instance is killed.
  val header = SPHeader(from = d.name)
  val body = api.TheDriver(d, streamToMap(urState))
  publish(api.topicResponse, SPMessage.makeJson(header, body))

  // All messages to the actor arrive here
  def receive = {
    // the stream from the dummy UR
    case x: URStream  => // matching the stream from the UR
      handleCmdDone(x)
      if (urState != x) log.debug(x.toString)
      sendStateToBus(streamToMap(urState))
      urState = x
      log.debug(s"${d.name} pos: ${x.currentPos}")

    case x: String =>
      // SPMessage uses the APIParser to parse the json string
      SPMessage.fromJson(x).foreach{mess =>
        for {
          h <- mess.getHeaderAs[SPHeader] // we can filter the message based on the header, but is skipping it for now
          b <- mess.getBodyAs[api.Request] // we are expecting a case class in the Vritual device API
        } yield {
          log.debug("URDRIVER req: " +b)
          b match {
            case api.GetDriver =>
              val body = api.TheDriver(d, streamToMap(urState))
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), body))
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), APISP.SPDone()))

            // The command to the driver
            case api.DriverCommand(driverid, state) if driverid == d.id  => // matching that it is a command and that it is to this driver
              handleCmd(state - "currentPos", h) // removing currentPos since that can not be set
                                                 //mediator ! Publish("driverEvents", SPMessage.makeJson(header, body))

            // Terminating the driver
            case api.TerminateDriver(driverid) if driverid == d.id =>
              self ! PoisonPill
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), api.DriverTerminated(d.id)))
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), APISP.SPDone()))
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
      value <- x.to[Boolean].toOption if urState.active != value
    } yield {
      val send = if (value) "activate" else "deactivate"
      myUR ! send
    }

    for {
      x <- state.get("hasTool")
      value <- x.to[Boolean].toOption if urState.hasTool != value
    } yield {
      val send = if (value) "addTool" else "removeTool"
      myUR ! send
    }

    for {
      x <- state.get("refPos")
      value <- x.to[Int].toOption if urState.refPos != value
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
        val b = api.DriverCommandDone(updH.reqID, true) // we do not check if it fails in this case
        publish(api.topicResponse, SPMessage.makeJson(updH, b))
        reqHeader = None
        changed = Map()
      }
    }
  }

  // Sending a message to the bus
  def sendStateToBus(state: Map[String, SPValue]) = {
    val updH = SPHeader(from = d.name)
    val b = api.DriverStateChange(d.name, d.id, state, false)
    publish(api.topicResponse, SPMessage.makeJson(updH, b))

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
class DummyUR(replyTo: ActorRef) extends Actor with ActorLogging {
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
      if (active && refPos != currentPos) {
        val diff = refPos-currentPos
        currentPos = currentPos + diff/Math.abs(diff)
      }
      // sending the state of the dummyUR every tick
      replyTo ! makeMess
  }

  def makeMess = {
    URStream(currentPos, refPos, active, hasTool)
  }

  // A "ticker" that sends a "tick" string to self every 0.2 second
  import context.dispatcher

  import scala.concurrent.duration._
  val ticker = context.system.scheduler.schedule(0.2 seconds, 0.2 seconds, self, "tick")
}

case class URStream(currentPos: Int, refPos: Int, active: Boolean, hasTool: Boolean)

object DummyUR {
  def props(replyTo: ActorRef) = Props(classOf[DummyUR], replyTo)
}
