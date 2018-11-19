package spgui.circuits.main

import diode.react.{ReactConnectProxy, ReactConnector}
import diode.{Action, Circuit, FastEq, ModelR}
import spgui.circuits.availablemodelscircuit._
import spgui.circuits.main.handlers._

case class FrontendState(
                          models: ModelHandlerState,
                          abilities: AbilityHandlerState,
                          virtualDevices: VDHandlerState
                        )


case class LocalAction(action: Action) extends Action

/**
  * A circuit that contains all the data related to virtual devices: Models, abilities, drivers, etc.
  */
object MainCircuit extends Circuit[FrontendState] with ReactConnector[FrontendState] {
  def readState[S <: AnyRef](f: FrontendState => S)(implicit feq: FastEq[_ >: S]): ModelR[FrontendState, S] = zoom(f)

  /**
    * Dispatch an action locally. This tells [[spgui.communication.CommunicationAPI.Communicator[S, A]] objects
    * to not propagate anything to the backend.
    */
  def localDispatch(action: Action): Unit = dispatch(LocalAction(action))

  /**
    * Gives a React component access to part of the circuit's state
    */
  def connectComponent[S <: AnyRef](f: FrontendState => S)(implicit feq: FastEq[_ >: S]): ReactConnectProxy[S] = connect(f)

  override protected def initialModel: FrontendState = {
    FrontendState(
      models = ModelHandler.initialState,
      abilities = AbilityHandler.initialState,
      virtualDevices = VDHandler.initialState
    )
  }

  private val handlers = {
    foldHandlers(
      new ModelHandler(zoomRW(_.models)((state, modelState) => state.copy(models = modelState))),
      new AbilityHandler(zoomRW(_.abilities)((state, abilityState) => state.copy(abilities = abilityState))),
      new VDHandler(zoomRW(_.virtualDevices)((state, vdState) => state.copy(virtualDevices = vdState)))
    )
  }

  override protected def actionHandler: HandlerFunction = handlers
}
