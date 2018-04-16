package sp.volvosim

import akka.actor._
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.drivers.DriverBase

import sp.devicehandler.{APIDeviceDriver => api}



/**
  * A driver for a simulated volvo robot
  */
object DummyVolvoRobotDriver {
  val driverType = "DummyVolvoRobotDriver"
  def props = DriverBase.props(driverType, DummyVolvoRobotDriverInstance.props)
}



/**
  * The actual driver instance answering the commands
  */
object DummyVolvoRobotDriverInstance {
  def props(d: VD.Driver) = Props(classOf[DummyVolvoRobotDriverInstance], d)
}

/**
  * This is  running one UR robot, but only as a dummy
  * The driver creates a dummy UR robot, shown below
  * you can set a reference and some states
  * @param d APIVirtualDevice.Driver The name, id, and setup of the driver
  */
class DummyVolvoRobotDriverInstance(d: VD.Driver) extends Actor
  with ActorLogging
  with sp.service.MessageBussSupport {


  subscribe(api.topicRequest)


  // the internal state of the UR-robot, Using a case class to simplify life
  var state = DummyVolvoRobotState(
    currentProgram = "",
    currentTime = 0,
    eStop = false,
    homePosition = true
  )


  // Fix to load programs via the Driver soon
  val programs = Map(
    "prog5" -> 5,
    "prog10" -> 10,
    "prog20" -> 20,
    "prog30" -> 30,
  )


  // the dummy UR that we will talk to with this driver
  // this will be defined based on the setup in the real system
  // in this case the dummy UR will send messages to us since we
  // are its parents in the actor hierarchy
  val device = context.actorOf(DummyVolvoRobot.props(self, programs))

  // We have started and is publishing that we exist
  // TODO: Also add messages when instance is killed.
  val header = SPHeader(from = d.name)
  val body = api.TheDriver(d, stateToMap(state))
  publish(api.topicResponse, SPMessage.makeJson(header, body))

  // All messages to the actor arrive here
  def receive = {
    // the stream from the dummy UR
    case x: DummyVolvoRobotState  => // matching the stream from the UR
      handleCmdDone(x)
      if (state != x) log.debug(x.toString)
      sendStateToBus(stateToMap(state))
      state = x
      log.debug(s"${d.name} pos: ${x.currentTime}")

    case x: String =>
      // SPMessage uses the APIParser to parse the json string
      SPMessage.fromJson(x).foreach{mess =>
        for {
          h <- mess.getHeaderAs[SPHeader] // we can filter the message based on the header, but is skipping it for now
          b <- mess.getBodyAs[api.Request] // we are expecting a case class in the Vritual device API
        } yield {
          log.debug("Volvo Simulation robot driver req: " +b)
          b match {
            case api.GetDriver =>
              val body = api.TheDriver(d, stateToMap(state))
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), body))
              publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), APISP.SPDone()))

            // The command to the driver
            case api.DriverCommand(driverid, s) if driverid == d.id  => // matching that it is a command and that it is to this driver
              handleCmd(s, h) // removing currentPos since that can not be set
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
  def handleCmd(cmd: Map[String, SPValue], h: SPHeader) = {
    // Setting up variables to check when the dummyUR is updated
    reqHeader = Some(h)
    changed = cmd

    // Converting to the actual api
    for {
      x <- cmd.get("currentProgram")
      value <- x.to[String].toOption if state.currentProgram != value || programs.contains(value)
    } yield {
      device ! DummyVolvoRobotStartProgram(value)
    }

    // Add more commands here, lite e-stop



  }

  def handleCmdDone(upd: DummyVolvoRobotState) = {
    reqHeader.foreach { header =>
      val streamMap = stateToMap(upd)
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

  def stateToMap(state: DummyVolvoRobotState) = {
    Map[String, SPValue](
      "currentProgram" -> state.currentProgram,
      "currentTime" -> state.currentTime,
      "eStop" -> state.eStop,
      "homePosition" -> state.homePosition
    )
  }




}
