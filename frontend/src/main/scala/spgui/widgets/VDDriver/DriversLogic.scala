package spgui.widgets.VDDriver

import sp.domain._
import sp.devicehandler.APIDeviceDriver
import sp.devicehandler._
import spgui.widgets.VDDriver.DriverCard

object DriversLogic {
  val drivers: List[VD.Driver] = List()
  val connection: Map[VD.Driver, DriverCardTrait] = Map()

  // drivers = getDrivers() -> Hämta från backend
  // För all VD.Driver i drivers, skapa ett DriverCard och Mapa mot sig självt

}

