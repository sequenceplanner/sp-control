package spgui.communication

import rx.Obs
import sp.devicehandler.{APIDeviceDriver, APIVirtualDevice}
import sp.domain._
import sp.models.{APIModel, APIModelMaker => ModelMaker}
import sp.vdtesting.APIVDTracker
import spgui.circuits.main.handlers.StateHandler
import spgui.circuits.main.{FrontendState, MainCircuit}

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
  onSocketChange(APIVirtualDevice.topicResponse) {
    VDCommunication.postRequest(APIVirtualDevice.GetVD)
  }

  onSocketChange(APIVDTracker.topicResponse) {
    VDTrackerCommunication.postRequest(APIVDTracker.getModelsInfo())
  }

  /* This is necessary because responseTopics are instantiated after the calls in the Communicator trait,
   * and will therefore be null
   */
  List(
    DriverCommunication,
    ModelCommunication,
    OperationRunnerCommunication,
    VDCommunication,
    VDTrackerCommunication
  ).foreach(_.startListening())

  DriverCommunication.postRequest(APIDeviceDriver.GetDrivers)

  type UnsubscribeFn = () => Unit

  def run(): Iterable[UnsubscribeFn] = {
    Seq(
      MainCircuit.subscribe(MainCircuit.readState(_.abilities)) { _ =>
        //println("1")
      },
      MainCircuit.subscribe(MainCircuit.readState(_.runners)) { _ =>
        //println("2")
      },
      MainCircuit.subscribe(MainCircuit.readState(_.virtualDevices)) { _ =>
        //println("3")
      },
      MainCircuit.subscribe(MainCircuit.readState(_.drivers)) { _ =>
        //println("4")
      }
    )
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
      BackendCommunication.getMessageObserver(onReceiveMessage, responseTopic)
    }

    def defaultReply: String

    protected def post[R](body: R, from: String, to: String, topic: String, reply: String = defaultReply, reqId: Option[ID] = None)(implicit writes: JSWrites[R]): Unit = {
      val headerBase = SPHeader(from = from, to = to, reply = SPValue(reply))
      val header = reqId match {
        case None => headerBase
        case Some(id) => headerBase.copy(reqID = id)
      }

      val json = SPMessage.make(header, body)
      BackendCommunication.publish(json, topic)
    }

    protected def onReceiveMessage(message: SPMessage): Unit

    def globalDispatch(action: Action): Unit = MainCircuit.dispatch(action)

    /**
      * Should be used when the dispatch should only trigger non-effectful changes. Practically,
      * this triggers the [[StateHandler.Reaction.stateTransform]] function
      * but not the [[StateHandler.GlobalReaction.global]] function.
      */
    def localDispatch(action: Action): Unit = MainCircuit.localDispatch(action)

    def responseTopic: String
  }
}
