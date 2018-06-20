package spgui

import sp.domain._
import spgui.availablemodelscircuit._
import spgui.communication.BackendCommunication
import sp.models.{APIModelMaker => ModelMaker}
import sp.models.{APIModel => Model}
import SPMessageUtil.BetterSPMessage
import play.api.libs.json.JsString

object ModelCommunication {
  println("ModelCommunication runs.")
  private val websocketObserver = BackendCommunication.getWebSocketStatusObserver(
    callBack = isConnected => if (isConnected) postRequest(ModelMaker.GetModels),
    topic = ModelMaker.topicResponse
  )

  private val topicHandler = BackendCommunication.getMessageObserver(message => handleMess(message), ModelMaker.topicResponse)

  type UnsubscribeFn = () => Unit

  def run(): Iterable[UnsubscribeFn] = {
    Seq({

      ModelsCircuit.subscribe(ModelsCircuit.readState) { reader =>
        val state = reader()
        println(s"-->\n\tprev: ${state.previousActiveModelId}\n\tcurr: ${state.activeModelId}")
        println(s"")
      }
    })
  }

  /**
    * Helper for dispatching actions on the model circuit
    */
  def dispatchAction[A](modifier: A => A, wrapper: A => ModelAction)(maybeA: Option[A]): Unit = {
    maybeA.foreach(a => ModelsCircuit.dispatch(wrapper(modifier(a))))
  }

  /**
    * Handles responses from a specific model.
    */
  def onModelResponse(state: ModelsCircuitState, header: SPHeader, res: Model.Response): Unit = {
    println(s"header: $header")

    res match {
      case info: Model.ModelInformation =>
        val value = state.models.get(info.id)
        dispatchAction(ModelMock.info.set(Some(info)), UpdateModel)(value)

      case Model.ModelHistory(id, history) =>
        val value = state.models.get(id)
        dispatchAction(ModelMock.history.set(Some(history)), UpdateModel)(value)


      case Model.SPItems(items) =>
        println("Model.SPItems")
        val modelId = JsString(header.from).asOpt[ID]
        modelId.foreach { id =>
          ModelsCircuit.dispatch(SetItems(id, items))
        }

      case Model.ModelUpdate(modelId, version, count, _, _, _) =>
        postRequest(modelId, Model.GetModelHistory)

        val value = state.models.get(modelId)
        dispatchAction(
          ModelMock.info.modify(_.map { info =>
            Model.ModelInformation(info.name, info.id, version, count, info.attributes)
          }),
          UpdateModel
        )(value)

      case x =>
        println(s"[ModelCommunication.onModelResponse] Case $x not captured by match.")
        Unit
    }
  }

  def onModelMakerResponse(state: ModelsCircuitState, res: ModelMaker.Response): Unit = {
    res match {
      case ModelMaker.ModelList(modelIds) =>
        modelIds.foreach { m =>
          postRequest(m, Model.GetModelInfo)
          postRequest(m, Model.GetModelHistory)
        }

        ModelsCircuit.dispatch(AddMockModelIds(modelIds))

      case created: ModelMaker.ModelCreated =>
        //sendToModel(modelId, Model.PutItems(TestModelInControl.getTestModel))
        ModelsCircuit.dispatch(AddMockModelIds(created.id))

      case ModelMaker.ModelDeleted(modelId) =>
        ModelsCircuit.dispatch(RemoveModel(modelId))

      case _ => Unit
    }
  }

  def handleMess(message: SPMessage): Unit = {
    val response = message.oneOf[ModelMaker.Response].or[Model.Response]
    val state = ModelsCircuit.readState()

    for (header <- response.header; body <- response.body) {
      body match {
        case res: ModelMaker.Response => onModelMakerResponse(state, res)
        case res: Model.Response => onModelResponse(state, header, res)
        case _ => Unit
      }
    }


  }

  /**
    * Post a request to the ModelMaker
    */
  def postRequest(request: ModelMaker.Request): Unit = post(request, ModelMaker.service)

  /**
    * Post a request to a specific model, given by the modelId
    */
  def postRequest(modelId: ID, request: Model.Request): Unit = post(request, modelId.toString)


  private def post[T](body: T, to: String)(implicit writes: JSWrites[T]): Unit = {
    val h = SPHeader(from = "ModelCommunication", to = to, reply = SPValue("ModelCommunication"))

    val json = SPMessage.make(h, body)
    BackendCommunication.publish(json, Model.topicRequest)
  }
}
