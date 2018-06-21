package spgui.communication

import sp.domain.SPMessage
import spgui.SPMessageUtil.BetterSPMessage
import spgui.availablemodelscircuit._

object VDTrackerCommunication extends CommunicationAPI.Communicator[AbilityAction] {
  import sp.vdtesting.{APIVDTracker => API}
  val responseTopic: String = API.topicResponse

  def onReceiveMessage(message: SPMessage): Unit = {
    import sp.vdtesting.{APIVDTracker => API}
    val response = message.as[API.Response]
    val state = VDCircuit.readModelState

    for ((header, body) <- response) {
      case API.sendModelsInfo(modelNames) =>

      case API.OpRunnerCreated(operationRunnerId) =>

    }
  }

  def postRequest(request: API.Request): Unit = {
    post(
      request,
      from = "VDTrackerCommunication",
      to = API.service,
      topic = API.topicRequest
    )
  }
}
