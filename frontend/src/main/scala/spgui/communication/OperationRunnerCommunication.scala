package spgui.communication

import sp.domain.SPMessage
import sp.runners.APIOperationRunner
import spgui.SPMessageUtil.BetterSPMessage
import spgui.availablemodelscircuit._

object OperationRunnerCommunication extends CommunicationAPI.Communicator[String, ModelAction] {
  import sp.runners.{APIOperationRunner => API}
  import API.Response.fOperationRunnerResponse

  def onReceiveMessage(message: SPMessage): Unit = {
    val response = message.as[API.Response]

    for ((header, body) <- response) body match {
        case API.StateEvent(runnerID, runnerState, runInAuto, disableConditionGroups) =>

        case API.Runner(setup) =>

        case API.Runners(setups) =>
    }
  }

  def postRequest(request: APIOperationRunner.Request): Unit = {
    post(
      request,
      from = "OperationRunnerCommunication",
      to = APIOperationRunner.service,
      topic = APIOperationRunner.topicRequest
    )
  }

  val responseTopic: String = APIOperationRunner.topicResponse
  override protected def stateAccessFunction: FrontendState => String = NoState
}
