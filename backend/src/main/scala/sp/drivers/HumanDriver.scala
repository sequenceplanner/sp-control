package sp.drivers

import akka.actor._
import akka.kafka.Subscriptions
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import org.apache.kafka.clients.producer.ProducerRecord
import sp.devicehandler.VD.DriverState
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler.{APIDeviceDriver => api}
import sp.driver.APIHumanDriver



/**
  * A driver for talking to human services, like widgets or a watch
  */
object HumanDriver {
  val driverType = "HumanDriver"
  def props = DriverBase.props(driverType, HumanDriverInstance.props)
}

object HumanDriverInstance {
  def props(d: VD.Driver) = Props(classOf[HumanDriverInstance], d)
}

class HumanDriverInstance(d: VD.Driver) extends Actor with KafkaStreamHelper
  with ActorLogging
  with sp.service.MessageBussSupport {

  override val system = context.system

  subscribe(api.topicRequest)
  subscribe(APIHumanDriver.topicFromHuman)

  // The name of the driver is the identification name of the human, for now.
  val name = d.name

  var driverState = Map[String, SPValue]()


  def receive = {
    case x: String =>
      SPMessage.fromJson(x).foreach{ mess =>
        for {
          h <- mess.getHeaderAs[SPHeader]
          b <- mess.getBodyAs[api.Request]
        } yield {
          val header = h.swapToAndFrom.copy(from = d.name)
          log.debug(s"HumanDRIVER req: " +b)
          b match {
              case api.GetDriver =>
                publish(api.topicResponse, SPMessage.makeJson(header, api.TheDriver(d, driverState)))
                publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, driverState)))
                publish(api.topicResponse, SPMessage.makeJson(header, APISP.SPDone()))

              case api.DriverCommand(driverid, state) if driverid == d.id  =>
                publish(api.topicResponse, SPMessage.makeJson(header, APISP.SPACK()))
                handleCmd(state, header)
                val myHeader = SPHeader(from = d.id.toString, to = d.name, reply = SPValue(h.reqID))
                val b = APIHumanDriver.StateChangeRequest(d.name, state)
                publish(APIHumanDriver.topicToHuman, SPMessage.makeJson(myHeader, b))

              // Terminating the driver
              case api.TerminateDriver(driverid) if driverid == d.id =>
                self ! PoisonPill
                publish(api.topicResponse, SPMessage.makeJson(header, api.DriverTerminated(d.id)))
                publish(api.topicResponse, SPMessage.makeJson(header, APISP.SPDone()))

              case _ =>
            }
          }

        for {
          h <- mess.getHeaderAs[SPHeader]
          b <- mess.getBodyAs[APIHumanDriver.FromHuman]
        } yield {
          b match {
            case APIHumanDriver.HumanEvent(name, s) if name == d.name =>
              driverState = driverState ++ s
              h.reply.to[ID].map{id =>
                val header = SPHeader(from = d.id.toString, to = id.toString, reply = h.reply)
                publish(api.topicResponse, SPMessage.makeJson(header, api.DriverCommandDone(id, true)))
                publish(api.topicResponse, SPMessage.makeJson(header, APISP.SPDone()))
              }
              publish(api.topicResponse, SPMessage.makeJson(SPHeader(from = d.id.toString), APIDeviceDriver.DriverStateChange(d.name, d.id, driverState)))

          }
        }
      }

  }

  // Mapping from state to actual dummy UR api
  def handleCmd(state: Map[String, SPValue], h: SPHeader) = {
    // send to kafka

  }







  /**
    * Listens for the pong for the last ping
    */



  // Sending a message to the bus
  def sendStateToBus(state: Map[String, SPValue]) = {
    val updH = SPHeader(from = d.name)
    val b = api.DriverStateChange(d.name, d.id, state, false)
    publish(api.topicResponse, SPMessage.makeJson(updH, b))

  }







}
