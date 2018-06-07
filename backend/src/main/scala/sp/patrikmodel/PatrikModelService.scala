package sp.patrikmodel

import akka.actor._
import sp.domain._
import sp.domain.Logic._
import sp.domain.logic.StructLogic._
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
        val spHeader = h.swapToAndFrom()
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPACK()))

        val toSend = commands(b) // doing the logic
        toSend.foreach(msg => sendAnswer(SPMessage.makeJson(spHeader, msg)))
        sendAnswer(SPMessage.makeJson(spHeader, APISP.SPDone()))
      }
  }

  def sendAnswer(mess: String) = publish(API.topicResponse, mess)
}

object PatrikModelService {
  def props = Props(classOf[PatrikModelService])
}

trait PatrikModelLogic extends SynthesizeModel {
  import modeledCases._
  val models = List(
    SimpleTest(),
    VolvoWeldConveyerCase(),
    GKNcase(),
    GKNSmallcase(),
    ROARcase(),
    PSLFloorRoofCase(),
    TrucksCase()
  )

  def addHierarchies(ids: List[IDAble]): List[IDAble] = {
    val h = ids.groupBy(_.attributes.getAs[Set[String]]("hierarchy").getOrElse(Set()).toList)
    val all = h.map {
      case (Nil,items) => items
      case (x :: Nil, items) =>
        val sns = items.map(i=>StructNode(i.id)).toSet
        val s = Struct(x, sns)
        s::items
      case (x :: xs, items) =>
        // todo
        val sns = items.map(i=>StructNode(i.id)).toSet
        val s = Struct(x, sns)
        s::items
    }
    all.toList.flatten
  }

  def commands(body: API.Request) = {
    body match {
      case API.GetAvailableModels =>
        List(API.AvailableModels(models.map(_.modelName)))
      case API.CreateManualModel(name) =>
        models.find(_.modelName == name).headOption match {
          case Some(model) =>
            val ids = model.parseToIDables
            val (ops,_,_) = synthesizeModel(ids)
            val upd = ids.filterNot(old=>ops.exists(_.id==old.id)) ++ ops
            // "hierarchies" are now structs
            val withH = addHierarchies(upd)

            List(API.ManualModel(withH))
          case None =>
            List()
        }
    }
  }
}
