package spgui.circuits.main

import diode.react.{ReactConnectProxy, ReactConnector}
import diode.{Action, Circuit, FastEq, ModelR}
import spgui.circuits.availablemodelscircuit._
import spgui.circuits.main.handlers._

case class FrontendState(
                          modelState: ModelsCircuitState,
                          driverState: DriverHandlerState,
                          abilityState: AbilityHandlerState,
                          runnerState: RunnerHandlerState
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
      modelState = ModelHandler.initialState,
      driverState = DriverHandler.initialState,
      abilityState = AbilityHandler.initialState,
      runnerState = RunnerHandler.initialState
    )
  }

  private val handlers = {
    composeHandlers(
      new ModelHandler(zoomRW(_.modelState)((state, modelState) => state.copy(modelState = modelState))),
      new DriverHandler(zoomRW(_.driverState)((state, driverState) => state.copy(driverState = driverState))),
      new AbilityHandler(zoomRW(_.abilityState)((state, abilityState) => state.copy(abilityState = abilityState))),
      new RunnerHandler(zoomRW(_.runnerState)((state, runnerState) => state.copy(runnerState = runnerState)))
    )
  }

  override protected def actionHandler: HandlerFunction = handlers
}