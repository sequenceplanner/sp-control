package spgui

import sp.domain._
import spgui.availablemodelscircuit._
import spgui.communication.BackendCommunication
import sp.models.{APIModel, APIModelMaker => ModelMaker}
import SPMessageUtil.BetterSPMessage
import play.api.libs.json.JsString
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.{APIDeviceDriver, APIVirtualDevice}
import sp.runners.APIOperationRunner
import sp.vdtesting.APIVDTracker

object ModelCommunication {
  private val websocketObserver = BackendCommunication.getWebSocketStatusObserver(
    callBack = isConnected => if (isConnected) Recipient.Model.postRequest(ModelMaker.GetModels),
    topic = APIModel.topicResponse
  )

  private val topicHandler = BackendCommunication.getMessageObserver(onReceiveMessage, APIModel.topicResponse)

  type UnsubscribeFn = () => Unit

  def run(): Iterable[UnsubscribeFn] = {
    Seq({

      SOPCircuit.subscribe(SOPCircuit.readModelState) { reader =>
        // val state = reader()
        /*
        println(s"-->\n\tprev: ${state.previousActiveModelId}\n\tcurr: ${state.activeModelId}")
        println(s"")
        */
      }
    })
  }

  /**
    * Helper for dispatching actions on the model circuit
    */
  def dispatchAction[A](modifier: A => A, wrapper: A => ModelAction)(maybeA: Option[A]): Unit = {
    maybeA.foreach(a => SOPCircuit.dispatch(wrapper(modifier(a))))
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
          SOPCircuit.dispatch(SetItems(id, items))
        }

      case APIModel.ModelUpdate(modelId, version, count, _, _, _) =>
        ModelCommunication.Recipient.Model.postRequest(modelId, APIModel.GetModelHistory)

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

  def onModelMakerResponse(state: ModelsCircuitState, res: ModelMaker.Response): Unit = {
    res match {
      case ModelMaker.ModelList(modelIds) =>
        modelIds.foreach { m =>
          Recipient.Model.postRequest(m, APIModel.GetModelInfo)
          Recipient.Model.postRequest(m, APIModel.GetModelHistory)
        }

        SOPCircuit.dispatch(AddMockModelIds(modelIds))

      case created: ModelMaker.ModelCreated =>
        //sendToModel(modelId, Model.PutItems(TestModelInControl.getTestModel))
        SOPCircuit.dispatch(AddMockModelIds(created.id))

      case ModelMaker.ModelDeleted(modelId) =>
        SOPCircuit.dispatch(RemoveModel(modelId))

      case _ => Unit
    }
  }

  def onReceiveMessage(message: SPMessage): Unit = {
    val response = message.oneOf[ModelMaker.Response].or[APIModel.Response]
    val state = SOPCircuit.readModelState()

    for (header <- response.header; body <- response.body) {
      body match {
        case res: ModelMaker.Response => onModelMakerResponse(state, res)
        case res: APIModel.Response => onModelResponse(state, header, res)
        case _ => Unit
      }
    }
  }

  trait Recipient {
    protected def post[R](body: R, from: String, to: String, topic: String)(implicit writes: JSWrites[R]): Unit = {
      val h = SPHeader(from = from, to = to, reply = SPValue("ModelCommunication"))

      val json = SPMessage.make(h, body)
      BackendCommunication.publish(json, topic)
    }
  }

  object Recipient {

    case object Model extends Recipient {
      def postRequest(modelId: ID, request: APIModel.Request, from: String)(implicit writes: JSWrites[APIModel.Request]): Unit = {
        val to = modelId.toString
        post(request, from, to, topic = APIModel.topicRequest)
      }

      def postRequest(modelId: ID, request: APIModel.Request)(implicit writes: JSWrites[APIModel.Request]): Unit = {
        postRequest(modelId, request, "ModelCommunication")
      }

      def postRequest(request: ModelMaker.Request, from: String): Unit = {
        post(
          request,
          from,
          to = ModelMaker.service,
          topic = ModelMaker.topicRequest
        )
      }

      def postRequest(request: ModelMaker.Request): Unit = postRequest(request, "ModelCommunication")
    }

    case object OperationRunner extends Recipient {
      def postRequest(request: APIOperationRunner.Request): Unit = {
        post(
          request,
          from = "VDTrackerWidget",
          to = APIOperationRunner.service,
          topic = APIOperationRunner.topicRequest
        )
      }
    }

    case object Device extends Recipient {
      def postRequest(request: APIVirtualDevice.Request): Unit = {
        post(
          request,
          from = "VDTrackerWidget",
          to = APIVirtualDevice.service,
          topic = APIVirtualDevice.topicRequest
        )
      }

      def postRequest(request: APIVDTracker.Request): Unit = {
        post(
          request,
          from = "VDTrackerWidget",
          to = APIVDTracker.service,
          topic = APIVDTracker.topicRequest
        )
      }

      def postRequest(request: APIDeviceDriver.Request): Unit = {
        post(
          request,
          from = "VDTrackerWidget",
          to = "DriverService",
          topic = APIDeviceDriver.topicRequest
        )
      }
    }

    case object AbilityHandler extends Recipient {
      def postRequest(request: APIAbilityHandler.Request): Unit = {
        post(
          request,
          from = "VDTrackerWidget",
          to = APIAbilityHandler.service,
          topic = APIAbilityHandler.topicRequest
        )
      }

    }

  }
}
