package spgui.communication

import sp.domain.SPMessage
import spgui.SPMessageUtil.BetterSPMessage
import spgui.availablemodelscircuit._

object DeviceCommunication extends CommunicationAPI.Communicator[AbilityAction] {
  import sp.devicehandler.{APIDeviceDriver => API}
  val responseTopic: String = API.topicResponse

  def onReceiveMessage(message: SPMessage): Unit = {
    val response = message.as[API.Response]
    val state = VDCircuit.readModelState

    for ((header, body) <- response) {
      case API.DriverCommandDone(requestId, result) =>

      case API.DriverStateChange(name, id, driverState) =>

      case API.TheDriver(driver, driverState) =>

      case API.TheDrivers(drivers) =>

    }
  }

  def postRequest(request: API.Request): Unit = {
    post(
      request,
      from = "VDTrackerWidget",
      to = "DriverService",
      topic = API.topicRequest
    )
  }
}
