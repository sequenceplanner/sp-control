package spgui.availablemodelscircuit

import diode.{Circuit, ModelR}
import diode.react.{ReactConnectProxy, ReactConnector}

case class FrontendState(modelState: ModelsCircuitState, driverState: DriverHandlerState, abilityState: AbilityHandlerState)

/**
  * A circuit that contains all the data related to virtual devices: Models, abilities, drivers, etc.
  */
object VDCircuit extends Circuit[FrontendState] with ReactConnector[FrontendState] {
  def readModelState: ModelR[FrontendState, ModelsCircuitState] = zoom(_.modelState)
  def connectModels: ReactConnectProxy[ModelsCircuitState] = connect(_.modelState)

  override protected def initialModel: FrontendState = FrontendState(
    modelState = ModelHandler.initialState,
    driverState = DriverHandler.initialState,
    abilityState = AbilityHandler.initialState
  )

  override protected def actionHandler: HandlerFunction = composeHandlers(
    new ModelHandler(zoomRW(_.modelState)((state, modelState) => state.copy(modelState = modelState))),
    new DriverHandler(zoomRW(_.driverState)((state, driverState) => state.copy(driverState = driverState))),
    new AbilityHandler(zoomRW(_.abilityState)((state, abilityState) => state.copy(abilityState = abilityState)))
  )
}