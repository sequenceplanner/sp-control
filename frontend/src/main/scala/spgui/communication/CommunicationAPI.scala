package spgui.communication

import play.api.libs.json.JsString
import rx.Obs
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.{APIDeviceDriver, APIVirtualDevice}
import sp.domain._
import sp.models.{APIModel, APIModelMaker => ModelMaker}
import sp.runners.APIOperationRunner
import sp.vdtesting.APIVDTracker
import spgui.availablemodelscircuit._
import spgui.SPMessageUtil.BetterSPMessage

/**
  * This class is meant to be used as the primary means of communication between frontend and backend.
  * It updates VDCircuit with data sent by the backend.
  */
object CommunicationAPI {
  private def webSocketObserver(topic: String)(onConnectionChanged: Boolean => Unit): Obs = {
    BackendCommunication.getWebSocketStatusObserver(
      callBack = onConnectionChanged,
      topic
    )
  }

  /**
    * Set up backend response handlers
    */
  private val messageObservers: List[Obs] = {
    import BackendCommunication.getMessageObserver
    val pairs = List[(Communicator, String)](
      (onReceiveModelMessage, APIModel.topicResponse),
      (onOperationRunnerMessage, APIOperationRunner.topicResponse),
      (onAbilityMessage, APIAbilityHandler.topicResponse),
      (onVirtualDeviceMessage, APIVirtualDevice.topicResponse),
      (onVirtualDeviceTrackerMessage, APIVDTracker.topicResponse),
      (onDriverMessage, APIDeviceDriver.topicResponse)
    )

    pairs.map { case (onMessage, topic) => getMessageObserver(onMessage, topic) }
  }

  /**
    * Set up websocket connection observers
    */
  webSocketObserver(APIModel.topicResponse) { isConnected =>
    if (isConnected) Communicator.Model.postRequest(ModelMaker.GetModels)
  }
  webSocketObserver(APIAbilityHandler.topicResponse) { isConnected =>
    if (isConnected) Communicator.AbilityHandler.postRequest(APIAbilityHandler.GetAbilities)
  }
  webSocketObserver(APIVirtualDevice.topicResponse) { isConnected =>
    if (isConnected) Communicator.Device.postRequest(APIVirtualDevice.GetVD)
  }

  Communicator.Device.postRequest(APIDeviceDriver.GetDrivers)

  type UnsubscribeFn = () => Unit

  def run(): Iterable[UnsubscribeFn] = {
    Seq({

      VDCircuit.subscribe(VDCircuit.readModelState) { reader =>
        // val state = reader()
        /*
        println(s"-->\n\tprev: ${state.previousActiveModelId}\n\tcurr: ${state.activeModelId}")
        println(s"")
        */
      }
    })
  }

  /**
    *
    * @tparam Action diode Action type that the communicator may dispatch
    */
  trait Communicator[Action <: diode.Action] {
    val messageObserver = BackendCommunication.getMessageObserver(onReceiveMessage, responseTopic)

    protected def post[R](body: R, from: String, to: String, topic: String)(implicit writes: JSWrites[R]): Unit = {
      val h = SPHeader(from = from, to = to, reply = SPValue("ModelCommunication"))

      val json = SPMessage.make(h, body)
      BackendCommunication.publish(json, topic)
    }

    def onReceiveMessage(message: SPMessage): Unit

    /**
      * Helper for dispatching actions on the model circuit
      */
    def dispatchAction[V](modifier: V => V, wrapper: V => Action)(maybeA: Option[V]): Unit = {
      maybeA.foreach(a => VDCircuit.dispatch(wrapper(modifier(a))))
    }

    def responseTopic: String
  }

  object Communicator {

    case object Model extends Communicator {
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

    case object OperationRunner extends Communicator {
      def postRequest(request: APIOperationRunner.Request): Unit = {
        post(
          request,
          from = "VDTrackerWidget",
          to = APIOperationRunner.service,
          topic = APIOperationRunner.topicRequest
        )
      }
    }

    case object Device extends Communicator {
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

    case object AbilityHandler extends Communicator {
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
