package sp.operationmatcher

import java.util.UUID
import java.util.concurrent.TimeUnit

import scala.concurrent.duration._

import akka.actor._
import akka.persistence._
import sp.domain._
import sp.domain.Logic._

object OperationMatcher {
  def props(name: String, id: UUID) = Props(classOf[OperationMatcher], name, id)
}

object OperationMatcherInfo {
  import sp.domain.SchemaLogic._

  case class OperationMatcherRequest(request: API.Request)
  case class OperationMatcherResponse(response: API.Response)

  val req: com.sksamuel.avro4s.SchemaFor[OperationMatcherRequest] = com.sksamuel.avro4s.SchemaFor[OperationMatcherRequest]
  val resp: com.sksamuel.avro4s.SchemaFor[OperationMatcherResponse] = com.sksamuel.avro4s.SchemaFor[OperationMatcherResponse]

  val apischema = makeMeASchema(
    req(),
    resp()
  )

  private val id = ID.newID
  val attributes: APISP.StatusResponse = APISP.StatusResponse(
    service = API.service,
    instanceID = Some(id),
    instanceName = s"OperationMatcher-$id",
    tags = List("operatoinmatcher", "runtime"),
    api = apischema,
    version = 1,
    attributes = SPAttributes.empty
  )
}

class OperationMatcher(val name: String, val id: UUID) extends PersistentActor
    with ActorLogging
    with sp.service.ServiceCommunicationSupport
    with sp.service.MessageBussSupport {
  import context.dispatcher
  import sp.abilityhandler.{ APIAbilityHandler => ahapi }

  var abilities: Set[ahapi.Ability] = Set()

  override def persistenceId = id.toString


  subscribe(API.topicRequest)
  subscribe(ahapi.topicResponse)

    // Setting up the status response that is used for identifying the service in the cluster
  val statusResponse = OperationMatcherInfo.attributes.copy(
    instanceID = Some(id)
  )
  // starts wiating for ping requests from service handler
  triggerServiceRequestComm(statusResponse)

  def sendToAH(ahid: ID, request: ahapi.Request) = {
    val msg = SPMessage.makeJson[SPHeader, ahapi.Request](SPHeader(to = ahapi.service, from = "hej"), request)
    publish(ahapi.topicRequest, msg)
  }

  def sendResponse(resp: API.Response) = {
    val msg = SPMessage.makeJson[SPHeader, API.Response](SPHeader(from = id.toString), resp)
    publish(API.topicResponse, msg)
  }

  override def receiveCommand = {
    case x: String =>
      val mess = SPMessage.fromJson(x)

      for {
        m <- mess
        h <- m.getHeaderAs[SPHeader]
        b <- m.getBodyAs[API.Request]
      } yield {
        b match {


          case x => println("todo: " + x)

        }
      }


      for {
        m <- mess
        h <- m.getHeaderAs[SPHeader]
        b <- m.getBodyAs[ahapi.Response]
      } yield {
        b match {
          case ahapi.Abilities(abs: List[ahapi.Ability]) =>
            abilities ++= abs.toSet
          case x =>
        }
      }

  }

  def receiveRecover = {
    case x: String =>

    case RecoveryCompleted =>

  }
}
