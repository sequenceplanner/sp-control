package spgui.communication

import play.api.libs.json.JsString
import sp.domain.{ID, JSWrites, SPHeader, SPMessage}
import sp.models.{APIModel, APIModelMaker}
import spgui.availablemodelscircuit._
import spgui.SPMessageUtil.BetterSPMessage
import CommunicationAPI.Communicator

object ModelCommunication extends Communicator[ModelsCircuitState, ModelAction] {
  println("ModelCommunication live :)")
    override def onReceiveMessage(message: SPMessage): Unit = {
      println("onReceiveMessage ModelCommunication")
      println(message)
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
  def onModelResponse(state: ModelsCircuitState, header: SPHeader, res: APIModel.Response): Unit = {
    println("onModelResponse")
    res match {
      case info: APIModel.ModelInformation =>
        val value = state.models.get(info.id)
        dispatchAction(ModelMock.info.set(Some(info)), UpdateModel)(value)

      case APIModel.ModelHistory(id, history) =>
        val value = state.models.get(id)
        dispatchAction(ModelMock.history.set(Some(history)), UpdateModel)(value)


      case APIModel.SPItems(items) =>
        val modelId = JsString(header.from).asOpt[ID]
        modelId.foreach { id =>
          dispatch(SetItems(id, items))
        }

      case APIModel.ModelUpdate(modelId, version, count, _, _, _) =>
        postRequest(modelId, APIModel.GetModelHistory)

        val value = state.models.get(modelId)
        dispatchAction(
          ModelMock.info.modify(_.map { info =>
            APIModel.ModelInformation(info.name, info.id, version, count, info.attributes)
          }),
          UpdateModel
        )(value)

      case x =>
        println(s"[ModelCommunication.onModelResponse] Case $x not captured by match.")
        Unit
    }
  }

  def onModelMakerResponse(state: ModelsCircuitState, res: APIModelMaker.Response): Unit = {
    println("onModelMakerResponse")
    res match {
      case APIModelMaker.ModelList(modelIds) =>
        modelIds.foreach { m =>
          postRequest(m, APIModel.GetModelInfo)
          postRequest(m, APIModel.GetModelHistory)
        }

        dispatch(AddMockModelIds(modelIds))

      case created: APIModelMaker.ModelCreated =>
        //sendToModel(modelId, Model.PutItems(TestModelInControl.getTestModel))
        dispatch(AddMockModelIds(created.id))

      case APIModelMaker.ModelDeleted(modelId) =>
        dispatch(RemoveModel(modelId))

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
    println("postRequest in Model")
    post(request, from, to = APIModelMaker.service, topic = APIModelMaker.topicRequest)
  }

  def postRequest(request: APIModelMaker.Request)(implicit writes: JSWrites[APIModel.Request]): Unit = postRequest(request, from = "ModelCommunication")

  override protected def stateAccessFunction: FrontendState => ModelsCircuitState = _.modelState
  val responseTopic: String = APIModel.topicResponse
}
