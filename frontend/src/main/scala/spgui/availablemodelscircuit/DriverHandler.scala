package spgui.availablemodelscircuit

import diode.{Action, ActionHandler, ActionResult, ModelRW}
import monocle.macros.Lenses
import sp.devicehandler.VD.{Driver, DriverState}
import spgui.availablemodelscircuit.DriverHandler.DriverInfo


trait DriverAction extends Action
case class AddDriver(driver: DriverInfo) extends DriverAction
case class AddDrivers(driver: List[DriverInfo]) extends DriverAction


object DriverHandler {
  type DriverStatus = String
  type DriverInfo = (Driver, DriverState, DriverStatus)

  val initialState: DriverHandlerState = DriverHandlerState(List())
}

@Lenses case class DriverHandlerState(drivers: List[DriverInfo])


class DriverHandler[M](modelRW: ModelRW[M, DriverHandlerState]) extends ActionHandler(modelRW) {
  import DriverHandlerState.drivers

  type StateFn = DriverHandlerState => DriverHandlerState

  private def handleAction: PartialFunction[DriverAction, StateFn] = {
    case AddDriver(driver) => drivers.modify(_ :+ driver)
    case AddDrivers(newDrivers) => drivers.modify(_ ++ newDrivers)
  }

  override protected def handle: PartialFunction[Any, ActionResult[M]] = {
    case action: DriverAction => updated(handleAction(action)(value))
  }
}