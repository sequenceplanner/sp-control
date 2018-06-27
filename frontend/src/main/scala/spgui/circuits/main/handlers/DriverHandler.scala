package spgui.circuits.main.handlers

import diode.{Action, ModelRW}
import monocle.macros.Lenses
import sp.devicehandler.VD.{Driver, DriverState}
import sp.domain.{ID, SPAttributes}
import spgui.SimpleSet
import spgui.circuits.main.handlers.DriverHandler.{DriverId, DriverStatus, RequestId}


trait DriverAction extends Action
case class AddDriver(driver: Driver, state: DriverState) extends DriverAction
case class AddDrivers(driver: List[(Driver, DriverState, DriverStatus)]) extends DriverAction
case class DriverTerminated(id: DriverId) extends DriverAction
case class DriverChanged(name: String, id: DriverId, state: DriverState) extends DriverAction

case class DriverRequest(id: RequestId, requests: List[RequestId]) {
  def -(id: RequestId): DriverRequest = copy(requests = requests.filter(_ != id))
}
@Lenses case class DriverHandlerState(drivers: SimpleSet[DriverId, DriverInfo])

// TODO Someone with domain knowledge needs to take a look at how updates happen.
// TODO It is probably incorrect in several places. For example, state might be
// TODO when it should actually be merged, etc.
class DriverHandler[M](modelRW: ModelRW[M, DriverHandlerState]) extends StateHandler[M, DriverHandlerState, DriverAction](modelRW) {
  import DriverHandlerState.drivers

  override def onAction: PartialFunction[DriverAction, Reaction] = {
      case AddDriver(driver, driverState) =>
        val newDriver = DriverInfo(driver, driverState)
        (drivers ^|-> SimpleSet.upsert(driver.id, newDriver)).modify(_.copy(state = driverState))

      case AddDrivers(newDrivers) =>
        drivers.modify(_.addAll(newDrivers.map(DriverInfo.tupled)))

      case DriverTerminated(driverId) =>
      drivers.modify(_.modifyByKey(_.terminate)(driverId))

      case DriverChanged(name, id, state) =>
        val updateDriver = (d: DriverInfo) => d.driverChange(_.copy(name = name, id = id))
        val updateState = (d: DriverInfo) => d.copy(state = state)

      drivers.modify(_.modifyByKey(updateDriver compose updateState)(id))

  }

  override def acceptAction: Action => Boolean = {
    case _: DriverAction => true
    case _ => false
  }
}

case class DriverInfo(
                       driver: Driver,
                       state: DriverState,
                       status: DriverStatus = DriverStatus.Unresponsive
                     ) {
  val name: String = driver.name
  val id: ID = driver.id
  val driverType: String = driver.driverType
  val setup: SPAttributes = driver.setup

  def terminate: DriverInfo = copy(status = DriverStatus.Terminated)
  def driverChange(f: Driver => Driver): DriverInfo = {
    copy(driver = f(driver))
  }
}


object DriverHandler {
  type DriverStatus = String
  object DriverStatus {
    val Online = "Online"
    val Offline  = "Offline"
    val Unresponsive  = "Unresponsive"
    val Terminated = "Terminated"
  }
  /**
    * Type for ID's of requests given to drivers
    */
  type RequestId = ID
  type DriverId = ID

  val initialState: DriverHandlerState = DriverHandlerState(new SimpleSet(_.id, Map()))
}