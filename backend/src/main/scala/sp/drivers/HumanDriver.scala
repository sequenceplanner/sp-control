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

import scala.util.{Failure, Success}










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



  // The state of the op
  var blueToothConnected  = false

  var humanName = "humanName"
  var humanID = "humanID"
  var loggedIn = "loggedIn"
  var cmd = "cmd"
  var ack = "ack"
  var done = "done"
  var alert = "alert"

  val defaultState = Map[String, SPValue](
    humanName -> "",
    humanID -> "",
    loggedIn -> false,
    cmd -> "",
    ack -> false,
    done -> false,
    alert -> ""
  )

  var driverState = defaultState

  def updS(key: String, v: SPValue) = driverState += (key -> v)

  // The commands that we send to the operator
  val instructions = d.setup.getAs[Map[String, String]]("instructions").getOrElse(Map())
  val bluetooth = context.actorOf(Props(classOf[BluetoothConnector]))

  subscribe(api.topicRequest)
  subscribe(APIHumanDriver.topicFromHuman)

  // The driver state is based on what we get back from the various human services.

  publish(api.topicResponse, SPMessage.makeJson(SPHeader(from = d.id.toString), api.TheDriver(d, driverState)))

  println("HUMANDRIVER *********************")
  println("started")



  def receive = {
    case BluetoothConnect =>
      blueToothConnected = true

    case x: GotMessage =>
      if (! (driverState.get(ack).contains(SPValue(x.ack)) && driverState.get(done).contains(SPValue(x.done)))){
        updS(ack, x.ack)
        updS(done, x.done)
        sendUpdState()
      }


    case "tick" =>
      sendUpdState()

    case x: String =>
      //println("HUMANDRIVER GOT:" + x)
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

                driverState = driverState ++ state
                //println("HUMAN DRIVER GOT new state: "+ driverState)

                sendUpdState()
//

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
            val header = h.swapToAndFrom(d.name)
            b match {
              case x: APIHumanDriver.HumanEvent if x.driverID == d.id =>
                if (driverState.get(ack).contains(SPValue(x.ack)) || driverState.get(done).contains(SPValue(x.done))){
                  updS(ack, x.ack)
                  updS(done, x.done)
                  sendUpdState()
                }

              case _ => println("GOT THIS FROM HUMAN: " + mess)
            }
          }

      }

  }



  import context.dispatcher

  import scala.concurrent.duration._
  val ticker = context.system.scheduler.schedule(1 seconds, 1 seconds, self, "tick")





  /**
    * Listens for the pong for the last ping
    */

  def sendUpdState() = {
    sendStateToBluetooth()
    sendStateToBus(driverState)
    sendStateToUI()
  }

  // Sending a message to the bus
  def sendStateToBus(state: Map[String, SPValue]) = {
    val updH = SPHeader(from = d.name)
    val b = api.DriverStateChange(d.name, d.id, state, false)
    publish(api.topicResponse, SPMessage.makeJson(updH, b))

  }

  def sendStateToBluetooth() = {
    val updH = SPHeader(from = d.name)
    val sA = SPAttributes.make(driverState)
    val cmdS = sA.getAs[String](cmd).get
    val ackS = sA.getAs[Boolean](ack).get
    val doneS = sA.getAs[Boolean](done).get
    val alertS = sA.getAs[String](alert).get
    val mess = SendMessage(cmdS, ackS, doneS, alert)

    if (blueToothConnected) bluetooth ! mess
  }

  def sendStateToUI() = {
    val updH = SPHeader(from = d.name)
    val sA = SPAttributes.make(driverState)
    val humanNameS = sA.getAs[String](humanName).get
    val humanIDS = sA.getAs[String](humanID).get
    val cmdS = sA.getAs[String](cmd).get
    val ackS = sA.getAs[Boolean](ack).get
    val doneS = sA.getAs[Boolean](done).get
    val loggedInS = sA.getAs[Boolean](loggedIn).get
    val alertS = sA.getAs[String](alert).get
    val b = APIHumanDriver.HumanStateMessage(
      d.id,
      humanNameS,
      humanIDS,
      loggedInS,
      cmdS,
      ackS,
      doneS,
      blueToothConnected,
      instructions,
      alertS
    )
    publish(APIHumanDriver.topicToHuman, SPMessage.makeJson(updH, b))

  }



}



case class SendMessage(cmd: String, ack: Boolean, done: Boolean, alert: String)
case class GotMessage(ack: Boolean, done: Boolean, alert: String)

import play.api.libs.json._
object SendMessage {
  implicit val fSendMess: JSFormat[SendMessage] = Json.format[SendMessage]
}
object GotMessage{
  implicit val fGotMessage: JSFormat[GotMessage] = Json.format[GotMessage]
}



import scala.util._
case object BluetoothConnect
class BluetoothConnector extends Actor {
  import context.dispatcher

  val blue = new Proxy(self ! _)

  blue.proxy match {
    case Success(p) => println("Blutooth started..................")
    case Failure(p) =>
      println("Blutooth failed..................")
      println(p.getLocalizedMessage)
  }

  context.parent ! BluetoothConnect


  override def receive = {
    case mess: String =>
      val x = SPValue.fromJson(mess)
      println("GOT from bluetooth: " + x)

      x.foreach(_.to[GotMessage].foreach { res =>
        println("We could convert the message from bluetooth")
        context.parent ! res
      })

    case x: SendMessage =>
      val mess = SPValue(x).toJson
      blue.proxy.map(_.send(mess))
  }
}

class Proxy(callBack: String => Unit) extends sp.bluetooth.BluetoothMessageListener{
  // This builds the Bluetooth Proxy. The process will block until
  // a device connects to it. Might fail if multiple devices try to
  // connect at the same time.
  println("before bluetooth")
  val proxy: Try[BluetoothProxy] = Failure(new UnsupportedOperationException()) //Try{new BluetoothProxy(this)}
  println(s"after: $proxy")

  // Use proxy's send method to send messages to the device
  //proxy.send("Hello! this is server")

  override def onBluetoothMessage(message: String): Unit = {
    println("[RECEIVED] " + message)
    callBack(message)
    //proxy.send("Received")
  }
}
