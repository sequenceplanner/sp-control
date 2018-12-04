package sp.modelSupport

import sp.models._

import akka.actor._
import sp.domain.Logic._
import sp.domain._
import sp.models.{APIModel, APIModelMaker}
import sp.service.MessageBussSupport
import scala.concurrent.duration._

import akka.stream._
import akka.stream.scaladsl._
import akka.NotUsed

import sp.runners._
import sp.runners.Shared._
import sp.runners.API._

object MiniModelService {
  def props = Props(classOf[MiniModelService])
}

class MiniModelService extends Actor with MessageBussSupport {
  import context.dispatcher

  val models = Map(
    "URTest" -> new unification.urdemo.Demo(context.system),
    "SDU" -> new sdu.Model(context.system),
    "Unification ROS2" -> new unification.UnificationModel(context.system),
    "NewExtendedDummy" -> new unification.NewExtended(context.system),
    "UnicornDemo" -> new unicorn.MondayDemo(context.system),
  )

  subscribe(APIModel.topicResponse)
  subscribe(APIModelMaker.topicResponse)
  subscribe(APIMiniModelService.topicRequest)


  def createModel(name: String, modelID: ID): Unit = {
    //val modelID = ID.makeID("0d80d1d6-48cd-48ec-bfb1-d69714ef35be").get // hardcoded model id so we do not get a new model every time

    val model = models(name)

    val idables = model.getIDAbles()
    val attributes = SPAttributes("isa" -> "VD")
    val newModel = sp.models.APIModelMaker.CreateModel(name, attributes, id = modelID)
    val rootNode = Struct(name, makeStructNodes(idables), attributes)
    val items = APIModel.PutItems(rootNode :: idables, SPAttributes("info" -> "initial items"))

    context.system.scheduler.scheduleOnce(0.1 seconds) {
      publish(
        APIModelMaker.topicRequest,
        SPMessage.makeJson(SPHeader(from = "MiniModelService", to = APIModelMaker.service), newModel)
      )
    }

    context.system.scheduler.scheduleOnce(1 seconds) {
      publish(
        APIModel.topicRequest,
        SPMessage.makeJson(SPHeader(from = "MiniModelService", to = newModel.id.toString), items)
      )
    }

    val resources = model.makeResources()
    val initState = model.getInitialState ++ resources.foldLeft(State.empty){case (s,r) => s++r.initialState}

    // start model
    val runner = SPRunner(
      model.operations,
      initState,
      Struct("statevars"), // TODO
      AbilityRunnerTransitions.abilityTransitionSystem)

    val setup = SetupRunnerInstance(ID.newID, model.getIDAbles, resources, runner)

    import akka.cluster.pubsub._
    val mediator = DistributedPubSub(context.system).mediator
    import DistributedPubSubMediator.{ Put, Send, Subscribe, Publish }
    mediator ! Publish(APIRunnerManager.topicRequest, setup)
  }

  def receive: Receive = {
    case s : String =>
      for { // unpack message
        message <- SPMessage.fromJson(s)
        header <- message.getHeaderAs[SPHeader] if  header.to == APIMiniModelService.service
        body <- message.getBodyAs[APIMiniModelService.Request]
      } yield {
        val responseHeader = header.swapToAndFrom()
        sendAnswer(SPMessage.makeJson(responseHeader, APISP.SPACK())) // acknowledge message received

        body match { // Check if the body is any of the following classes, and execute program
          case APIMiniModelService.createModel(modelName, modelID) =>
            createModel(modelName, modelID)

          case APIMiniModelService.getModelsInfo =>
            sendAnswer(SPMessage.makeJson(responseHeader, APIMiniModelService.sendModelsInfo(models.keys.toList)))

          case _ => Unit
        }
        sendAnswer(SPMessage.makeJson(responseHeader, APISP.SPDone()))
      }
  }
  def sendAnswer(mess: String): Unit = publish(APIMiniModelService.topicResponse, mess)
}
