package spgui.availablemodelscircuit

import diode.{Circuit, ModelR}
import diode.react.{ReactConnectProxy, ReactConnector}

case class FrontendState(modelState: ModelsCircuitState)

/**
  * A circuit that contains all the data related to SOP: Models, abilities, drivers, etc.
  */
object SOPCircuit extends Circuit[FrontendState] with ReactConnector[FrontendState] {
  def readModelState: ModelR[FrontendState, ModelsCircuitState] = zoom(_.modelState)
  def connectModels: ReactConnectProxy[ModelsCircuitState] = connect(_.modelState)

  override protected def initialModel: FrontendState = FrontendState(
    modelState = ModelHandler.initialState
  )

  override protected def actionHandler: SOPCircuit.HandlerFunction = composeHandlers(
    new ModelHandler(zoomRW(_.modelState)((state, modelState) => state.copy(modelState = modelState)))
  )
}