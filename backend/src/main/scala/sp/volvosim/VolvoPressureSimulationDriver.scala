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
object VolvoPressureSimulationDriver {
  val driverType = "VolvoPressureSimulationDriver"
  def props = DriverBase.props(driverType, VolvoPressureSimulationInstance.props)
}



/**
  * The actual driver instance answering the commands
  */
object VolvoPressureSimulationInstance{
  def props(d: VD.Driver) = Props(classOf[VolvoPressureSimulationInstance], d)
}

/**
  * This is  running one UR robot, but only as a dummy
  * The driver creates a dummy UR robot, shown below
  * you can set a reference and some states
  * @param d APIVirtualDevice.Driver The name, id, and setup of the driver
  */
class VolvoPressureSimulationInstance(d: VD.Driver) extends Actor
  with ActorLogging
  with sp.service.MessageBussSupport {


  subscribe(api.topicRequest)


  // the internal state of the UR-robot, Using a case class to simplify life
  var state = PressureState(
    act = 0,
    ref = 0,
    atRef = true
  )



  val device = context.actorOf(DummyVolvoPressurizer.props(self))

  // We have started and is publishing that we exist
  // TODO: Also add messages when instance is killed.
  val header = SPHeader(from = d.name)
  val body = api.TheDriver(d, stateToMap(state))
  publish(api.topicResponse, SPMessage.makeJson(header, body))

  def receive = {
    case x: PressureState  =>
      handleCmdDone(x)
      if (state != x) log.debug(x.toString)
      sendStateToBus(stateToMap(state))
      state = x
      log.debug(s"${d.name} pos: ${stateToMap(x)}")

    case x: String =>
      // SPMessage uses the APIParser to parse the json string
      SPMessage.fromJson(x).foreach{mess =>
        for {
          h <- mess.getHeaderAs[SPHeader] // we can filter the message based on the header, but is skipping it for now
          b <- mess.getBodyAs[api.Request] // we are expecting a case class in the Vritual device API
        } yield {
          log.debug("Volvo Simulation pressure driver req: " +b)
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

  var reqHeader: Option[SPHeader] = None
  var changed: Map[String, SPValue] = Map()

  def handleCmd(cmd: Map[String, SPValue], h: SPHeader) = {
    reqHeader = Some(h)
    changed = cmd

    // Converting to the actual api
    for {
      x <- cmd.get("ref")
      value <- x.to[Int].toOption if state.ref != value
    } yield {
      if (value != 0)
        device ! Pressurize(value)
      else
        device ! DePressurize()
    }


    // Add more commands here, lite e-stop



  }

  def handleCmdDone(upd: PressureState) = {
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

  def stateToMap(state: PressureState): Map[String, SPValue] = {
    Map[String, SPValue](
      "act" -> state.act,
      "ref" ->  state.ref,
      "atRef" -> state.atRef
    )

  }




}





case class Pressurize(ref: Int)
case class DePressurize()
case class PressureState(act: Int, ref: Int, atRef: Boolean)

class DummyVolvoPressurizer(replyTo: ActorRef) extends Actor with ActorLogging {
  var state = PressureState(0,0, true)

  def receive = {
    case x: Pressurize =>
      state = state.copy(ref = x.ref)
    case x: DePressurize =>
      state = state.copy(ref = 0)


    case "tick" =>
      if (state.ref != state.act){
        val diff = state.ref-state.act
        val currentPos = state.act + diff/Math.abs(diff)
        state = state.copy(act = currentPos, atRef = state.ref == currentPos)
      }

      replyTo ! state
  }

  import context.dispatcher

  import scala.concurrent.duration._
  val ticker = context.system.scheduler.schedule(0 seconds, 1 seconds, self, "tick")
}


object DummyVolvoPressurizer {
  def props(replyTo: ActorRef) = Props(classOf[DummyVolvoPressurizer], replyTo)
}