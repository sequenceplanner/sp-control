package spgui.communication

import sp.domain.SPMessage
import spgui.SPMessageUtil.BetterSPMessage
import spgui.circuits.main.handlers._
import spgui.circuits.main.FrontendState

object DriverCommunication extends CommunicationAPI.Communicator[DriverHandlerState, DriverAction] {
  println("DeviceCommunication live :)")
  import sp.devicehandler.{APIDeviceDriver => API}
  val responseTopic: String = API.topicResponse

  def onReceiveMessage(message: SPMessage): Unit = {
    val response = message.as[API.Response]
    val state = currentState()

    for ((_, body) <- response) body match {
        // TODO Maybe use diff? Not sure what to use it for (6/22/2018)
      case API.DriverStateChange(name, id, driverState, _) =>
        localDispatch(DriverChanged(name, id, driverState))

      case API.TheDriver(driver, driverState) =>
        localDispatch(AddDriver(driver, driverState))

      case API.TheDrivers(drivers) =>
        localDispatch(AddDrivers(drivers))

      case API.DriverTerminated(id) =>
        localDispatch(DriverTerminated(id))

      case x =>
        println(s"[WARN] Received command $x that has no implemented case.")
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

  override def defaultReply: String = "DeviceCommunication"
}
