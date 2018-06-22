package spgui.communication

import diode.FastEq
import rx.Obs
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.{APIDeviceDriver, APIVirtualDevice}
import sp.domain._
import sp.models.{APIModel, APIModelMaker => ModelMaker}
import spgui.availablemodelscircuit._

/**
  * This class is meant to be used as the primary means of communication between frontend and backend.
  * It updates [[MainCircuit]] with data sent by the backend.
  */
object CommunicationAPI {
  private def onSocketChange(topic: String)(onConnectionChanged: => Unit): Obs = {
    BackendCommunication.getWebSocketStatusObserver(
      callBack = if (_) onConnectionChanged,
      topic
    )
  }

  /**
    * Set up websocket connection observers
    */
  onSocketChange(APIModel.topicResponse) {
    ModelCommunication.postRequest(ModelMaker.GetModels)
  }
  onSocketChange(APIAbilityHandler.topicResponse) {
    AbilityCommunication.postRequest(APIAbilityHandler.GetAbilities)
  }
  onSocketChange(APIVirtualDevice.topicResponse) {
    VDCommunication.postRequest(APIVirtualDevice.GetVD)
  }

  /* This is necessary because responseTopics are instantiated after the calls in the Communicator trait,
   * and will therefore be null
   */
  List(
    AbilityCommunication,
    DeviceCommunication,
    ModelCommunication,
    OperationRunnerCommunication,
    VDCommunication,
    VDTrackerCommunication
  ).foreach(_.startListening())

  DeviceCommunication.postRequest(APIDeviceDriver.GetDrivers)

  type UnsubscribeFn = () => Unit

  def run(): Iterable[UnsubscribeFn] = {
    Seq({

      MainCircuit.subscribe(MainCircuit.readState(k => k)) { reader =>
        // val state = reader()
      }
    })
  }

  /**
    *
    * @tparam State State for the diode action handler associated with the Communicator
    * @tparam Action diode Action type that the communicator may dispatch
    */
  trait Communicator[State <: AnyRef, Action <: diode.Action] {
    protected def stateAccessFunction: FrontendState => State
    protected def currentState(): State = {
      val reader = MainCircuit.readState(stateAccessFunction)
      reader()
    }

    protected val NoState: FrontendState => String = _ => "Unused"

    def startListening(): Obs = {
      println(s"messageObserver for $responseTopic")
      BackendCommunication.getMessageObserver(onReceiveMessage, responseTopic)
    }

    protected def post[R](body: R, from: String, to: String, topic: String)(implicit writes: JSWrites[R]): Unit = {
      println("Posting!")
      val h = SPHeader(from = from, to = to, reply = SPValue("ModelCommunication"))

      val json = SPMessage.make(h, body)
      BackendCommunication.publish(json, topic)
    }

    protected def onReceiveMessage(message: SPMessage): Unit

    /**
      * Helper for dispatching actions on the model circuit
      */
    def dispatchAction[V](modifier: V => V, wrapper: V => Action)(maybeA: Option[V]): Unit = {
      maybeA.foreach(a => MainCircuit.dispatch(wrapper(modifier(a))))
    }

    def dispatch(action: Action): Unit = MainCircuit.dispatch(action)
    def localDispatch(action: Action): Unit = MainCircuit.localDispatch(action)

    def responseTopic: String
  }
}
