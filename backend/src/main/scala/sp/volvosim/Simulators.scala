package sp.volvosim

import akka.actor._
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._

import sp.devicehandler.{APIDeviceDriver => api}


case class DummyVolvoRobotState(
  currentProgram : String,
  currentTime : Int,
  eStop : Boolean,
  homePosition : Boolean
)

case class DummyVolvoRobotStartProgram(prog: String)



class DummyVolvoRobot(replyTo: ActorRef, robotprogram:Map[String, Int]) extends Actor with ActorLogging {
  var state = DummyVolvoRobotState("", 0, false, true)

  def receive = {
    case DummyVolvoRobotStartProgram(prog) if state.currentProgram.isEmpty && robotprogram.contains(prog) =>
      println(s"Vi fick $prog")
      state = state.copy(currentProgram = prog)
      replyTo ! state

    case "tick" =>
      if (state.currentProgram.nonEmpty) {
        state = state.copy(currentTime = state.currentTime + 1)

        println(s"vårt state $state")

        if (robotprogram.get(state.currentProgram).exists(_ <= state.currentTime)){
          state = state.copy(currentProgram = "")
          state = state.copy(currentTime = 0)
          println(s"vårt state nu $state")
        }

      }
      // sending the state of the robot every tick
      replyTo ! state
  }

  // A "ticker" that sends a "tick" string to self every 0.2 second
  import context.dispatcher

  import scala.concurrent.duration._
  val ticker = context.system.scheduler.schedule(0 seconds, 1 seconds, self, "tick")
}


object DummyVolvoRobot {
  def props(replyTo: ActorRef, programs: Map[String, Int]) = Props(classOf[DummyVolvoRobot], replyTo, programs)
}





// Transport

case class BodyTracker(id: String, length: Int, frontPos: Int, rearPos: Int)

case class TransporterState(bodies: Map[String, BodyTracker],
                            run: Boolean,
                            sensorInput: Boolean,
                            sensorOutput: Boolean,
                                )

case class TransportDefinition(length: Int,
                               posInputSensor: Int,
                               posOutputSensor: Int,
                               nextTransporter: Option[ActorRef]
                              )

case class StartTransport()
case class StopTransport()
case class NewBody(bodyID: String, length: Int)
case class LeavingBody(bodyID: String, length: Int)


/**
  * A dummy UR robot that has one joint that it will update based on the refPos
  * Will only move if it is in the active state
  */
class DummyVolvoTransporter(replyTo: ActorRef, setup: TransportDefinition) extends Actor with ActorLogging {
  var state = TransporterState(Map(), false, false, false)

  def receive = {
    case x: StartTransport =>
      state = state.copy(run = true)
    case x: StopTransport =>
      state = state.copy(run = false)
    case x: NewBody =>
      val b = BodyTracker(
        id = x.bodyID,
        length = x.length,
        frontPos = 1,
        rearPos = -1)
      state = state.copy(bodies = state.bodies + (x.bodyID -> b))

    case x: LeavingBody =>
      val b = BodyTracker(
        id = x.bodyID,
        length = x.length,
        frontPos = 1,
        rearPos = -1)
      state = state.copy(bodies = state.bodies + (x.bodyID -> b))

    case "tick" =>
      if (state.) {
        state = state.copy(currentTime = state.currentTime + 1)

        println(s"vårt state $state")

        if (robotprogram.get(state.currentProgram).exists(_ <= state.currentTime)){
          state = state.copy(currentProgram = "")
          state = state.copy(currentTime = 0)
          println(s"vårt state nu $state")
        }

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
  def props(replyTo: ActorRef, programs: Map[String, Int]) = Props(classOf[DummyVolvoRobot], replyTo, programs)
}