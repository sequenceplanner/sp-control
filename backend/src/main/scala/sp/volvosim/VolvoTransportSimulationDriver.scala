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
object VolvoTransportSimulationDriver {
  val driverType = "VolvoTransportSimulationDriver"
  def props = DriverBase.props(driverType, VolvoTransportSimulationInstance.props)
}



/**
  * The actual driver instance answering the commands
  */
object VolvoTransportSimulationInstance{
  def props(d: VD.Driver) = Props(classOf[VolvoTransportSimulationInstance], d)
}

/**
  * This is  running one UR robot, but only as a dummy
  * The driver creates a dummy UR robot, shown below
  * you can set a reference and some states
  * @param d APIVirtualDevice.Driver The name, id, and setup of the driver
  */
class VolvoTransportSimulationInstance(d: VD.Driver) extends Actor
  with ActorLogging
  with sp.service.MessageBussSupport {


  subscribe(api.topicRequest)


  // the internal state of the UR-robot, Using a case class to simplify life
  var state = TransporterState(
    bodies = Map(),
    run = false,
    sensorList = List(),
    startcmd = false
  )


  val transportLength = d.setup.getAs[Int]("length").getOrElse(30)
  val transportSensors = d.setup.getAs[List[Sensor]]("sensors").getOrElse(
    List(Sensor("s1", 2, false), Sensor("s2", 28, false))
  )

  val transDef = TransportDefinition(
    length = transportLength,
    sensorList = transportSensors
  )

  val device = context.actorOf(DummyVolvoTransporter.props(self, transDef))

  // We have started and is publishing that we exist
  // TODO: Also add messages when instance is killed.
  val header = SPHeader(from = d.name)
  val body = api.TheDriver(d, stateToMap(state))
  publish(api.topicResponse, SPMessage.makeJson(header, body))

  def receive = {
    case x: TransporterState  =>
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

  var reqHeader: Option[SPHeader] = None
  var changed: Map[String, SPValue] = Map()

  def handleCmd(cmd: Map[String, SPValue], h: SPHeader) = {
    reqHeader = Some(h)
    changed = cmd

    // Converting to the actual api
    for {
      x <- cmd.get("start")
      value <- x.to[Boolean].toOption
    } yield {
      if (value)
        device ! StartTransport()
      else
        device ! StopTransport()
    }

    // Converting to the actual api
    for {
      x <- cmd.get("newBodyID")
      y <- cmd.get("newBodyLength")
      id <- x.to[String].toOption if !state.bodies.contains(id)
      length <- y.to[Int].toOption
    } yield {
      device ! NewBody(id, length)
    }

    // Add more commands here, lite e-stop



  }

  def handleCmdDone(upd: TransporterState) = {
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

  def stateToMap(state: TransporterState): Map[String, SPValue] = {

    val sensors = state.sensorList.map(s => s.id -> SPValue(s.active)).toMap

    val getB = if (state.bodies.isEmpty)
      Map("empty" -> BodyTracker("empty", -1, -1))
    else state.bodies
    
    val bodies = getB.flatMap{b =>
      Map("bodyID" -> SPValue(b._1), "bodyPos"->SPValue(b._2.frontPos))
    }

    sensors ++ bodies ++  Map[String, SPValue](
      "running" -> state.run,
      "start" ->  state.startcmd
    )

  }




}






// Transport

case class BodyTracker(id: String, length: Int, frontPos: Int)
case class Sensor(id: String, pos: Int, active: Boolean = false)
case object Sensor {
  import play.api.libs.json._
  implicit lazy val fAbility: JSFormat[Sensor] = Json.format[Sensor]
}

case class TransporterState(bodies: Map[String, BodyTracker],
                            run: Boolean,
                            sensorList: List[Sensor],
                            startcmd: Boolean
                           )

case class TransportDefinition(length: Int,
                               sensorList: List[Sensor]
                              )

case class StartTransport()
case class StopTransport()
case class NewBody(bodyID: String, length: Int)


/**
  * A dummy UR robot that has one joint that it will update based on the refPos
  * Will only move if it is in the active state
  */
class DummyVolvoTransporter(replyTo: ActorRef, setup: TransportDefinition) extends Actor with ActorLogging {
  var state = TransporterState(Map(), false, setup.sensorList, false)

  def receive = {
    case x: StartTransport =>
      state = state.copy(run = true, startcmd = true)
    case x: StopTransport =>
      state = state.copy(run = false, startcmd = false)
    case x: NewBody =>
      val b = BodyTracker(
        id = x.bodyID,
        length = x.length,
        frontPos = 0)
      state = state.copy(bodies = state.bodies + (x.bodyID -> b))


    case "tick" =>
      if (state.run){
        val updB = state.bodies.flatMap{ case (name, b) =>
          val front =  b.frontPos + 1
          if (front-b.length > setup.length) None else Some((name, b.copy(frontPos = front)))
        }

        val updS = state.sensorList.map {s =>
          val isActive = updB.foldLeft(false){(a, b) =>
            val car = b._2
            car.frontPos >= s.pos && (car.frontPos-car.length <= s.pos) || a
          }
          s.copy(active = isActive)
        }

        state = state.copy(bodies = updB, sensorList = updS)

      }

      // sending the state of the robot every tick
      replyTo ! state
  }

  // A "ticker" that sends a "tick" string to self every 0.2 second
  import context.dispatcher

  import scala.concurrent.duration._
  val ticker = context.system.scheduler.schedule(0 seconds, 1 seconds, self, "tick")
}


object DummyVolvoTransporter {
  def props(replyTo: ActorRef, setup: TransportDefinition) = Props(classOf[DummyVolvoTransporter], replyTo, setup)
}
