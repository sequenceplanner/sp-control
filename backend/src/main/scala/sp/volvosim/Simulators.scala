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

case class BodyTracker(id: String, length: Int, frontPos: Int)
case class Sensor(pos: Int, active: Boolean)

case class TransporterState(bodies: Map[String, BodyTracker],
                            run: Boolean,
                            sensorList: List[Sensor]
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
  var state = TransporterState(Map(), false, setup.sensorList)

  def receive = {
    case x: StartTransport =>
      state = state.copy(run = true)
    case x: StopTransport =>
      state = state.copy(run = false)
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