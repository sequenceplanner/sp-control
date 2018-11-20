package spgui.communication

import sp.domain.SPMessage
import spgui.SPMessageUtil.BetterSPMessage
import spgui.circuits.main.handlers.{AddRunner, UpdateRunnerState, VDAction}
import spgui.circuits.main.FrontendState

object RunnerManagerCommunication extends CommunicationAPI.Communicator[String, VDAction] {
  import sp.runners.{APIRunnerManager => API}
  val responseTopic: String = API.topicResponse

  def onReceiveMessage(message: SPMessage): Unit = {
    val response = message.as[API.Response]

    for ((_, body) <- response) body match {
      case API.RunnerStarted(id) =>
        localDispatch(AddRunner(id))

      case API.StateEvent(runnerID, stateData) =>
        localDispatch(UpdateRunnerState(runnerID, stateData))

      case API.TerminatedRunnerInstance(id) =>
        // TODO What should be done here?

      case API.TerminatedAllRunnerInstances =>
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
