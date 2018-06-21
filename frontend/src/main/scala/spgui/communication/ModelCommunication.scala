package spgui.communication

import play.api.libs.json.JsString
import sp.domain.{ID, JSWrites, SPHeader, SPMessage}
import sp.models.{APIModel, APIModelMaker}
import spgui.availablemodelscircuit._
import spgui.communication.CommunicationAPI.Communicator
import spgui.SPMessageUtil.BetterSPMessage

object ModelCommunication extends CommunicationAPI.Communicator[ModelAction] {
  val responseTopic: String = APIModel.topicResponse
  override def onReceiveMessage(message: SPMessage): Unit = {
    val response = message.oneOf[APIModelMaker.Response].or[APIModel.Response]
    val state = VDCircuit.readModelState()

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
          VDCircuit.dispatch(SetItems(id, items))
        }

      case APIModel.ModelUpdate(modelId, version, count, _, _, _) =>
        CommunicationAPI.Communicator.Model.postRequest(modelId, APIModel.GetModelHistory)

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
    res match {
      case APIModelMaker.ModelList(modelIds) =>
        modelIds.foreach { m =>
          Communicator.Model.postRequest(m, APIModel.GetModelInfo)
          Communicator.Model.postRequest(m, APIModel.GetModelHistory)
        }

        VDCircuit.dispatch(AddMockModelIds(modelIds))

      case created: APIModelMaker.ModelCreated =>
        //sendToModel(modelId, Model.PutItems(TestModelInControl.getTestModel))
        VDCircuit.dispatch(AddMockModelIds(created.id))

      case APIModelMaker.ModelDeleted(modelId) =>
        VDCircuit.dispatch(RemoveModel(modelId))

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
    post(
      request,
      from,
      to = APIModelMaker.service,
      topic = APIModelMaker.topicRequest
    )
  }

  def postRequest(request: APIModelMaker.Request): Unit = postRequest(request, from = "ModelCommunication")
}
