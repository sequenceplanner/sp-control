package spgui.widgets.VDDriverOld

import sp.devicehandler.VD.DriverState
import sp.domain._
import sp.domain.Logic._
import sp.devicehandler._

object DriversLogic {
  var drivers: List[VD.Driver] = List() // List of all drivers
  var driverCards: Map[VD.Driver, DriverCardTrait] = Map() // Map each driver with a DriverCard

  /**********API-COMMUNICATION**********/
  def getDrivers(): List[VD.Driver] = {
    // Call API DriverService for getDrivers
    List()

  }

  def removeDriverFromList(driverToRemove: VD.Driver): Unit = {
    drivers = drivers.filterNot(d => d.id == driverToRemove.id)
  }

  val driverHandler =
    BackendCommunication.getMessageObserver(onDriverMessage, APIDeviceDriver.topicResponse)

  /*
       for all drivers in list, create a new driverCard and map it with map with the Driver
    */
  def onDriverMessage(mess: SPMessage): Unit = {
    mess.body.to[APIDeviceDriver.Response].map{
      case APIDeviceDriver.DriverTerminated(id) => {

        // update driver-list and driverCards-map
      }
      case APIDeviceDriver.DriverStateChange(name, id, state, _) => {
        // update card with new state info1
      }
    }
  }

  /*
      create a new driver with APIDeviceDriver.SetUpDriver
   */
  def createDriver() = ???

  def terminateDriver(name: String, id: ID, driverType: String, setup: SPAttributes = _, state: DriverState = _) = ???

  /**********ACTIONS**********/
  /*
       for all drivers in list, create a new driverCard and map it with map with the Driver
    */
  def createDriverCards(): Map[VD.Driver, DriverCardTrait] = ???

  /*
        update the driver-list,
        alternative update the diffs / changes that have been made since last time updated
     */
  def updateDriverList() = ???





}

