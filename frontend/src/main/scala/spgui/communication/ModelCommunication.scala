package spgui.communication

import play.api.libs.json.JsString
import sp.domain.{ID, JSWrites, SPHeader, SPMessage}
import sp.models.{APIModel, APIModelMaker}
import spgui.SPMessageUtil.BetterSPMessage
import CommunicationAPI.Communicator
import spgui.circuits.main.handlers._
import spgui.circuits.main.FrontendState

object ModelCommunication extends Communicator[ModelHandlerState, ModelAction] {
    override def onReceiveMessage(message: SPMessage): Unit = {
      val response = message.oneOf[APIModelMaker.Response].or[APIModel.Response]
      val state = currentState()

      for ((header, body) <- response.get) {
        body match {
          case res: APIModelMaker.Response => onModelMakerResponse(state, res)
          case res: APIModel.Response => onModelResponse(state, header, res)
          case _ => Unit
        }
      }
    }

  /**
    * Handles responses from a specific model.
    */
  def onModelResponse(state: ModelHandlerState, header: SPHeader, res: APIModel.Response): Unit = {
    res match {
      case info: APIModel.ModelInformation =>
        val value = state.models.get(info.id)
        value.foreach(localDispatch _ compose UpdateModel compose ModelMock.info.set(Some(info)))

      case APIModel.ModelHistory(id, history) =>
        state.models.get(id).foreach(localDispatch _ compose UpdateModel compose ModelMock.history.set(Some(history)))


      case APIModel.SPItems(items) =>
        JsString(header.from).asOpt[ID].foreach(id => localDispatch(SetItems(id, items)))

      case APIModel.ModelUpdate(modelId, newVersion, newCount, _, _, _) =>
        postRequest(modelId, APIModel.GetModelHistory)

        val value = state.models.get(modelId)
        val updateInfo = ModelMock.info.modify(_.map(_.copy(version = newVersion, noOfItems = newCount)))

        value.foreach(localDispatch _ compose UpdateModel compose updateInfo)

      case x =>
        println(s"[ModelCommunication.onModelResponse] Case $x not captured by match.")
        Unit
    }
  }

  def onModelMakerResponse(state: ModelHandlerState, res: APIModelMaker.Response): Unit = {
    res match {
      case APIModelMaker.ModelList(modelIds) =>
        modelIds.foreach { m =>
          postRequest(m, APIModel.GetModelInfo)
          postRequest(m, APIModel.GetModelHistory)
        }

        localDispatch(AddMockModelIds(modelIds))

      case created: APIModelMaker.ModelCreated =>
        //sendToModel(modelId, Model.PutItems(TestModelInControl.getTestModel))
        localDispatch(AddMockModelIds(created.id))

      case APIModelMaker.ModelDeleted(modelId) =>
        localDispatch(RemoveModel(modelId))

      case _ => Unit
    }
  }

  def postRequest(modelId: ID, request: APIModel.Request, from: String)(implicit writes: JSWrites[APIModel.Request]): Unit = {
    val to = modelId.toString
    post(request, from, to, topic = APIModel.topicRequest)
  }

  def postRequest(modelId: ID, request: APIModel.Request)(implicit writes: JSWrites[APIModel.Request]): Unit = {
    postRequest(modelId, request, from = "ModelCommunication")
  }

  def postRequest(request: APIModelMaker.Request, from: String): Unit = {
    post(request, from, to = APIModelMaker.service, topic = APIModelMaker.topicRequest)
  }

  def postRequest(request: APIModelMaker.Request)(implicit writes: JSWrites[APIModel.Request]): Unit = postRequest(request, from = "ModelCommunication")

  override protected def stateAccessFunction: FrontendState => ModelHandlerState = _.models
  val responseTopic: String = APIModel.topicResponse

  override def defaultReply: String = "ModelCommunication"
}
