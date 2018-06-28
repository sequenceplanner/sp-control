package spgui.communication

import sp.domain.SPMessage
import spgui.SPMessageUtil.BetterSPMessage
import spgui.circuits.main.handlers.{AddVirtualDevice, UpdateResource, VDAction}
import spgui.circuits.main.FrontendState

object VDCommunication extends CommunicationAPI.Communicator[String, VDAction] {
  import sp.devicehandler.{APIVirtualDevice => API}
  val responseTopic: String = API.topicResponse

  def onReceiveMessage(message: SPMessage): Unit = {
    val response = message.as[API.Response]

    for ((_, body) <- response) body match {
      case API.StateEvent(_, resourceId, stateData, fullReplace) =>
        localDispatch(UpdateResource(resourceId, stateData, fullReplace))

      case API.TheVD(name, id, resources, drivers, attributes) =>
        localDispatch(AddVirtualDevice(name, id, resources, drivers, attributes))

      case API.TerminatedVD(id) =>
        // TODO What should be done here?

      case API.TerminatedAllVDs =>
      // TODO What should be done here?
    }
  }

  def postRequest(request: API.Request): Unit = {
    post(
      request,
      from = "VDCommunication",
      to = API.service,
      topic = API.topicRequest
    )
  }

  override protected def stateAccessFunction: FrontendState => String = NoState

  override def defaultReply: String = "VDCommunication"
}
