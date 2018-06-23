package spgui.communication

import sp.domain.SPMessage
import spgui.SPMessageUtil.BetterSPMessage
import spgui.circuits.main.handlers.{ModelNames, RunnerCreated, VDAction}
import spgui.circuits.main.FrontendState

object VDTrackerCommunication extends CommunicationAPI.Communicator[String, VDAction] {
  import sp.vdtesting.{APIVDTracker => API}
  val responseTopic: String = API.topicResponse

  def onReceiveMessage(message: SPMessage): Unit = {
    import sp.vdtesting.{APIVDTracker => API}
    val response = message.as[API.Response]

    for ((_, body) <- response) body match {
      case API.sendModelsInfo(modelNames) =>
        localDispatch(ModelNames(modelNames))

      case API.OpRunnerCreated(operationRunnerId) =>
        localDispatch(RunnerCreated(operationRunnerId))
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

  override protected def stateAccessFunction: FrontendState => String = NoState

  override def defaultReply: String = "VDTrackerCommunication"
}
