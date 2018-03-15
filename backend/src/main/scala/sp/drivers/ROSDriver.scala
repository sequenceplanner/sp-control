package sp.drivers

import akka.actor._
import akka.kafka.Subscriptions
import akka.kafka.scaladsl.{Consumer, Producer}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import org.apache.kafka.clients.producer.ProducerRecord
import sp.devicehandler._
import sp.domain.Logic._
import sp.domain._
import sp.devicehandler.{APIDeviceDriver => api}



/**
  * A driver for talking to ROS over kafka. Launch this actor and send it a
  * SetUpDeviceDriver with driverIdentifier = ROSDriver.driverType
  */
object ROSDriver {
  val driverType = "ROSDriver"
  def props = DriverBase.props(driverType, ROSDriverInstance.props)
}



/**
  * The actual driver instance answering the commands
  */
object ROSDriverInstance {
  def props(d: VD.Driver) = Props(classOf[ROSDriverInstance], d)
}

/**
  * This is  running one UR robot, but only as a dummy
  * The driver creates a dummy UR robot, shown below
  * you can set a reference and some states
  * @param d APIVirtualDevice.Driver The name, id, and setup of the driver
  */
class ROSDriverInstance(d: VD.Driver) extends Actor with KafkaStreamHelper
  with ActorLogging
  with sp.service.MessageBussSupport {

  override val system = context.system

  subscribe(api.topicRequest)

  val resourceName = "hmn_unidriver"
  val command = "command"
  val ur_state = "human"

  var driverState = Map[String, SPValue](
    command -> "",
    ur_state -> ""
  )


  // We have started and is publishing that we exist
  // TODO: Also add messages when instance is killed.
  val header = SPHeader(from = d.name)
  val body = api.TheDriver(d, driverState)
  publish(api.topicResponse, SPMessage.makeJson(header, body))
  publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, driverState)))




  val sendQueue = Source.queue[SPAttributes](Int.MaxValue, akka.stream.OverflowStrategy.backpressure)
    .map{ msg =>
      println("kafka sending: "+ msg)
      val mess = SPAttributes(
        "op" -> "publish",
        "topic" -> "/sp_to_driver",
        "msg" -> SPAttributes(
          "data" -> msg.toJson
      ))
      new ProducerRecord[Array[Byte], String]("sp_to_ros", mess.toJson)
    }
    .to(Producer.plainSink(producerSettings, kafkaProducer))
    .run()


  val fromKafka = Consumer.plainSource(consumerSettings, Subscriptions.topics("ros_to_sp"))
    .map{x =>
      println("sp receiving" + x.value)
      SPAttributes.fromJson(x.value).toOption
    }
      .collect{ case Some(xs) =>
        xs.getAs[ Map[String, Map[String, SPValue]]]("msg")
      }
      .collect{case Some(theMap) => ROSState(theMap)}
    .runWith(Sink.actorRefWithAck(self, "start", "ack", "terminate"))



  case class ROSState(xs: Map[String, Map[String, SPValue]])

  // All messages to the actor arrive here
  def receive = {
    // the stream from the dummy UR

    case ROSState(xs) =>
      xs.get(resourceName).foreach { s =>
        driverState = driverState ++ s
      }
      println("OUR STATE HAS CHANGED:")
      println(driverState)
      publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, driverState)))
      sender() ! "ack"


    case "start" => println("The kafka consumer is running")
    case "terminate" => println("The kafka consumer is dying")


    case x: String =>
      println("**********      ***************")
      println(x)
      println("**********      ***************")
      SPMessage.fromJson(x).foreach{mess =>
          for {
            h <- mess.getHeaderAs[SPHeader]
            b <- mess.getBodyAs[api.Request]
          } yield {
            log.debug(s"ROSDRIVER $resourceName  req: " +b)
            b match {
              case api.GetDriver =>
                val body = api.TheDriver(d, driverState)
                publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), body))
                publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, driverState)))
                publish(api.topicResponse, SPMessage.makeJson(h.swapToAndFrom.copy(from = d.name), APISP.SPDone()))


              // The command to the driver
              case api.DriverCommand(driverid, state) if driverid == d.id  => // matching that it is a command and that it is to this driver
                handleCmd(state, h) // removing currentPos since that can not be set
                //mediator ! Publish("driverEvents", SPMessage.makeJson(header, body))
                publish(api.topicResponse, SPMessage.makeJson(header, APIDeviceDriver.DriverStateChange(d.name, d.id, driverState)))

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

  // Mapping from state to actual dummy UR api
  def handleCmd(state: Map[String, SPValue], h: SPHeader) = {
    // Setting up variables to check when the dummyUR is updated
    reqHeader = Some(h)

    // send to kafka
    val msg = (SPAttributes(
      "receiver" -> resourceName
    ) ++ SPAttributes.make(state))

    sendQueue.offer(msg)

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
