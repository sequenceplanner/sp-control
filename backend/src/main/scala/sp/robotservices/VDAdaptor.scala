package sp.robotservices

import java.text.SimpleDateFormat

import akka.actor.{Actor, ActorLogging, ActorRef}
import sp.domain.{APISP, ID, SPHeader, SPMessage}
import com.codemettle.reactivemq._
import com.codemettle.reactivemq.ReActiveMQMessages._
import com.codemettle.reactivemq.model._



class VDAdaptor extends Actor with ActorLogging with RoutineExtractorLogic with
  sp.service.ServiceSupport {
  val instanceID = ID.newID


  // Setting up the status response that is used for identifying the service in the cluster
  val statusResponse = InstructionFillerInfo.attributes.copy(
    instanceID = Some(this.instanceID)
  )
  triggerServiceRequestComm(statusResponse)

  subscribe(APIRobotServices.topicRequest)


  //val log = Logging(context.system, getClass.getName)

  // State
  var theBus: Option[ActorRef] = None
  ReActiveMQExtension(context.system).manager ! GetConnection(s"nio://${APIRobotServices.activeMQUrl}:${APIRobotServices.activeMQPort}")


  def receive = {
    // enable the line below for troubleshooting
    //case mess @ _ if {println(s"ExampleService MESSAGE: $mess from $sender"); false} => Unit
    case ConnectionEstablished(request, c) =>
      log.info("Connected: " + request)
      c ! ConsumeFromTopic(APIRobotServices.activeMQTopic)
      theBus = Some(c)
    case ConnectionFailed(request, reason) =>
      log.error("Connection failed: " + reason)
    case mess @ AMQMessage(body, prop, headers) =>
      publish(APIRobotServices.topic,SPMessage.makeJson(SPHeader(),body.toString))

    case x: String =>
      // extract the body if it is a case class from my api as well as the header.to has my name
      // act on the messages from the API. Always add the logic in a trait to enable testing
      val bodyAPI = for {
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader] if  h.to == APIRobotServices.vdService // only extract body if it is to me
        b <- mess.getBodyAs[APIRobotServices.Request]
      } yield {
        //val spHeader = h.swapToAndFrom
        //sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
        //theBus !
        }

  }


  def sendToBusWithTopic(topic: String, json: String) = {
    theBus.foreach{bus => bus ! SendMessage(Topic(topic), AMQMessage(json))}
  }

  def sendToBus(json: String) = {
    theBus.foreach{bus => bus ! SendMessage(Topic(s"${APIRobotServices.activeMQTopic}"), AMQMessage(json))}
  }

  override def postStop() = {
    theBus.foreach(_ ! CloseConnection)
  }
}


object VDAdaptorInfo {
  import sp.domain.SchemaLogic._

  import sp.domain._

  case class VDAdaptorRequest(request: APIRobotServices.Request)
  case class VDAdaptorMessage(response: APIRobotServices.Message)

  lazy val req: com.sksamuel.avro4s.SchemaFor[VDAdaptorRequest] = com.sksamuel.avro4s.SchemaFor[VDAdaptorRequest]
  lazy val resp: com.sksamuel.avro4s.SchemaFor[VDAdaptorMessage] = com.sksamuel.avro4s.SchemaFor[VDAdaptorMessage]

  val apischema = makeMeASchema(
    req(),
    resp()
  )
  val attributes: APISP.StatusResponse = APISP.StatusResponse(
    service = "VDAdaptor",
    tags = List("VDAdaptor"),
    api = apischema,
    version = 1,
    topicRequest = APIRobotServices.topicRequest,
    topicResponse = APIRobotServices.topic,
    attributes = SPAttributes.empty
  )
}
