package spgui.communication

import sp.domain.SPMessage
import sp.runners.APIOperationRunner
import spgui.SPMessageUtil.BetterSPMessage
import spgui.circuits.main.handlers.{CreateRunner, CreateRunners, RunnerAction, UpdateRunner}
import spgui.circuits.main.FrontendState

object OperationRunnerCommunication extends CommunicationAPI.Communicator[String, RunnerAction] {
  import sp.runners.{APIOperationRunner => API}

  import API.Response.fOperationRunnerResponse

  def onReceiveMessage(message: SPMessage): Unit = {
    val response = message.as[API.Response]

    for ((_, body) <- response) body match {
        case API.StateEvent(runnerID, operationStates, runInAuto, disableConditionGroups) =>
          localDispatch(UpdateRunner(runnerID, operationStates, runInAuto, disableConditionGroups))

        case API.Runner(setup) =>
          localDispatch(CreateRunner(setup))

        case API.Runners(setups) =>
        localDispatch(CreateRunners(setups))
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

  override def defaultReply: String = "OperationRunnerCommunication"
}
