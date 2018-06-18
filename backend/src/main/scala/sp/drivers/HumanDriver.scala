package sp.drivers

import akka.actor._
import akka.kafka.Subscriptions
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import org.apache.kafka.clients.producer.ProducerRecord
import sp.bluetooth.{BluetoothProxy, ProxyApplication}
import sp.devicehandler.VD.DriverState
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler.{APIDeviceDriver => api}
import sp.driver.APIHumanDriver



class Proxy extends sp.bluetooth.BluetoothMessageListener{
  // This builds the Bluetooth Proxy. The process will block until
  // a device connects to it. Might fail if multiple devices try to
  // connect at the same time.
  println("before bluetooth")
  val proxy = new BluetoothProxy(this)
  println(s"after: $proxy")

  // Use proxy's send method to send messages to the device
  proxy.send("Hello! this is server")

  override def onBluetoothMessage(message: String): Unit = {
    println("[RECEIVED] " + message)

    proxy.send("Received")
  }
}






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

class HumanDriverInstance(d: VD.Driver) extends Actor
  with ActorLogging
  with sp.service.MessageBussSupport{


  private var proxy = null
  private var message_count = 0

  subscribe(api.topicRequest)
  subscribe(APIHumanDriver.topicFromHuman)

  // The name of the driver is the identification name of the human, for now.
  val name = d.name

  // The driver state is based on what we get back from the various human services.
  var driverState = Map[String, SPValue]()

  publish(api.topicResponse, SPMessage.makeJson(SPHeader(from = d.id.toString), api.TheDriver(d, driverState)))

  def receive = {
    case x: String =>
      println("HUMANDRIVER GOT:" + x)
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
                val myHeader = SPHeader(from = d.id.toString, to = d.name, reply = SPAttributes(
                  "reqID" -> h.reqID, "from" -> h.from, "reply" -> h.reply
                ))

                val updState = state ++ Map(
                  "cmd" -> state.getOrElse("cmd", SPValue("no command")),
                  "ack" -> state.getOrElse("ack", SPValue(false)),
                  "completed" -> state.getOrElse("completed", SPValue(false))
                )

                val b = APIHumanDriver.StateChangeRequest(d.name, updState)
                publish(APIHumanDriver.topicToHuman, SPMessage.makeJson(myHeader, b))
//
                println("KKKKKKKKKKKKKKKKKKKKKKKKKKKKKK")
                println( myHeader, b)
                println("KKKKKKKKKKKKKKKKKKKKKKKKKKKKKK")

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
            case he: APIHumanDriver.HumanEvent =>
              println("EEEEEEEEEEEEEEEEEEEEEEEEEE")
              println(he.state , b)
              println("EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEE")
              driverState = driverState ++ he.state

              for {
                attr <- h.reply.to[SPAttributes].toOption
                reqID <- attr.getAs[ID]("reqID")
                from <- attr.getAs[String]("from")
                reply <- attr.getAs[SPValue]("reply")
              } yield {
                val header = SPHeader(from = d.id.toString, to = from, reqID = reqID, reply = reply)
                publish(api.topicResponse, SPMessage.makeJson(header, api.DriverCommandDone(reqID, true)))
                publish(api.topicResponse, SPMessage.makeJson(header, APISP.SPDone()))

              }

              publish(api.topicResponse, SPMessage.makeJson(SPHeader(from = d.id.toString), APIDeviceDriver.DriverStateChange(d.name, d.id, driverState)))

          }
        }
      }

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
