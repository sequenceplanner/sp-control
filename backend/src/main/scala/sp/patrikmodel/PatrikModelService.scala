package sp.patrikmodel

import akka.actor._
import sp.domain._
import sp.domain.Logic._
import sp.service._

object PatrikModelServiceInfo {
  import sp.domain.SchemaLogic._
  case class PatrikModelRequest(request: API.Request)
  case class PatrikModelResponse(response: API.Response)

  lazy val req: com.sksamuel.avro4s.SchemaFor[PatrikModelRequest] = com.sksamuel.avro4s.SchemaFor[PatrikModelRequest]
  lazy val resp: com.sksamuel.avro4s.SchemaFor[PatrikModelResponse] = com.sksamuel.avro4s.SchemaFor[PatrikModelResponse]

  val apischema = makeMeASchema(
    req(),
    resp()
  )

  val attributes: APISP.StatusResponse = APISP.StatusResponse(
    service = API.service,
    tags = List("virtcom", "model", "manual"),
    api = apischema,
    version = 1,
    topicRequest = API.topicRequest,
    topicResponse = API.topicResponse,
    attributes = SPAttributes.empty
  )
}


class PatrikModelService extends Actor
    with ActorLogging
    with PatrikModelLogic
    with ServiceCommunicationSupport
    with MessageBussSupport {
  val instanceID = ID.newID

  val statusResponse = PatrikModelServiceInfo.attributes.copy(
    instanceID = Some(this.instanceID)
  )
  triggerServiceRequestComm(statusResponse)

  subscribe(API.topicRequest)

  def receive = {
    case x: String =>
      for {
        mess <- SPMessage.fromJson(x)
        h <- mess.getHeaderAs[SPHeader] if h.to == instanceID.toString || h.to == API.service
        b <- mess.getBodyAs[API.Request]
      } yield {
        println("GOT REQUEST: " + x)
        val spHeader = h.swapToAndFrom
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))

        val toSend = commands(b) // doing the logic
        toSend.foreach(msg => sendAnswer(SPMessage.makeJson(spHeader, msg)))
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))
      }
  }

  def sendAnswer(mess: String) = publish(API.topicResponse, mess)
}

object PatrikModelService {
  def props = Props(classOf[PatrikModelService])
}

trait PatrikModelLogic {
  import modeledCases._
  val models = List(
    VolvoWeldConveyerCase(),
    GKNcase(),
    GKNSmallcase(),
    ROARcase(),
    PSLFloorRoofCase(),
    TrucksCase()
  )

  def commands(body: API.Request) = {
    body match {
      case API.GetAvailableModels =>
        List(API.AvailableModels(models.map(_.modelName)))
      case API.CreateManualModel(name) =>
        models.find(_.modelName == name).headOption match {
          case Some(model) =>
            val pm = sp.patrikmodel.modeledCases.GKNSmallcase()
            val ids = sp.patrikmodel.CollectorModelImplicits.CollectorModelWorker(pm).parseToIDables
            List(API.ManualModel(ids))
          case None =>
            List()
        }
    }
  }
}
