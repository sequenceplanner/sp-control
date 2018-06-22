package spgui.availablemodelscircuit

import diode.{Action, Circuit, FastEq, ModelR}
import diode.react.{ReactConnectProxy, ReactConnector}

case class FrontendState(modelState: ModelsCircuitState, driverState: DriverHandlerState, abilityState: AbilityHandlerState)


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
      modelState = ModelHandler.initialState,
      driverState = DriverHandler.initialState,
      abilityState = AbilityHandler.initialState
    )
  }

  private val handlers = {
    composeHandlers(
      new ModelHandler(zoomRW(_.modelState)((state, modelState) => state.copy(modelState = modelState))),
      new DriverHandler(zoomRW(_.driverState)((state, driverState) => state.copy(driverState = driverState))),
      new AbilityHandler(zoomRW(_.abilityState)((state, abilityState) => state.copy(abilityState = abilityState)))
    )
  }

  override protected def actionHandler: HandlerFunction = handlers
}