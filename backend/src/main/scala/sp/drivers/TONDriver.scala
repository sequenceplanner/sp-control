package sp.drivers

import akka.actor._
import scala.concurrent.duration._
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler.{APIDeviceDriver => api}


/**
  * A driver to use for delaying something. For example in a sequence
  */
object TONDriver {
  val driverType = "TONDriver"
  def props = DriverBase.props(driverType, TONDriverInstance.props)
}

object TONDriverInstance {
  def props(d: VD.Driver) = Props(classOf[TONDriverInstance], d)
}

class TONDriverInstance(d: VD.Driver) extends Actor
  with ActorLogging
  with sp.service.MessageBussSupport{

  import context.dispatcher
  subscribe(api.topicRequest)

  var in = false // start on positive edge
  var pt = 1000 // the time in miliseconds
  var q = false // when time has passed

  def driverState = Map[String, SPValue](
    "in" -> in,
    "pt" -> SPValue(pt),
    "q" -> q
  )

  var timer: Option[Cancellable] = None
  def cancelTimer() = {
    timer.map(_.cancel())
    timer = None
  }

  def newCMD(s: Map[String, SPValue]) = {
    val sA = SPAttributes.make(s)
    val newIN = sA.getAs[Boolean]("in").getOrElse(in)
    val newPT = sA.getAs[Int]("pt").getOrElse(pt)

    println("AAAAAAAAAAAAAAAAA")
    println(driverState)
    println(s)
    println(sA)
    println("newIN: " + newIN)


    pt = newPT

    if (!newIN) {
      timer.map(_.cancel())
      in = false
      q = false
    } else if (newIN && !in){
      cancelTimer() // should never happen, but just in case
      timer = Some(
        context.system.scheduler.scheduleOnce(pt milliseconds, self, "timerDone")
      )
      in = true
      q = false
    }

    println("upd s: " + driverState)

  }



  publish(api.topicResponse, SPMessage.makeJson(SPHeader(from = d.id.toString), api.TheDriver(d, driverState)))

  def receive = {
    case "timerDone" =>
      q = true
      cancelTimer()

    case x: String =>
      SPMessage.fromJson(x).foreach{ mess =>
        for {
          h <- mess.getHeaderAs[SPHeader]
          b <- mess.getBodyAs[api.Request]
        } yield {
          val header = h.swapToAndFrom(d.name)
          b match {
              case api.GetDriver =>
                publish(api.topicResponse, SPMessage.makeJson(header, api.TheDriver(d, driverState)))
                publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, driverState)))
                publish(api.topicResponse, SPMessage.makeJson(header, APISP.SPDone()))

              case api.DriverCommand(driverid, state) if driverid == d.id  =>
                publish(api.topicResponse, SPMessage.makeJson(header, APISP.SPACK()))
                val myHeader = SPHeader(from = d.id.toString, to = d.name, reply = SPAttributes(
                  "reqID" -> h.reqID, "from" -> h.from, "reply" -> h.reply
                ))

                newCMD(state)
                publish(api.topicResponse, SPMessage.makeJson(header, api.DriverCommandDone(h.reqID, true)))
                sendUpdState()
//

              // Terminating the driver
              case api.TerminateDriver(driverid) if driverid == d.id =>
                cancelTimer()
                self ! PoisonPill
                publish(api.topicResponse, SPMessage.makeJson(header, api.DriverTerminated(d.id)))
                publish(api.topicResponse, SPMessage.makeJson(header, APISP.SPDone()))

              case _ =>
            }
          }
      }
  }


  def sendUpdState() = {
    val updH = SPHeader(from = d.name)
    val b = api.DriverStateChange(d.name, d.id, driverState, false)
    publish(api.topicResponse, SPMessage.makeJson(updH, b))
  }


}

