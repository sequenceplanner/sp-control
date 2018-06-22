package spgui.communication

import sp.domain.SPMessage
import spgui.SPMessageUtil.BetterSPMessage
import spgui.availablemodelscircuit._

object DeviceCommunication extends CommunicationAPI.Communicator[DriverHandlerState, AbilityAction] {
  println("DeviceCommunication live :)")
  import sp.devicehandler.{APIDeviceDriver => API}
  val responseTopic: String = API.topicResponse

  def onReceiveMessage(message: SPMessage): Unit = {
    val response = message.as[API.Response]
    val state = currentState()

    for ((header, body) <- response) body match {
      case API.DriverCommandDone(requestId, result) =>

      case API.DriverStateChange(name, id, driverState, diff) =>

      case API.TheDriver(driver, driverState) =>

      case API.TheDrivers(drivers) =>

      case API.DriverTerminated(id) =>

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

  override protected def stateAccessFunction: FrontendState => DriverHandlerState = _.driverState
}
