package spgui.communication

import sp.domain.SPMessage
import sp.runners.APIOperationRunner
import spgui.SPMessageUtil.BetterSPMessage
import spgui.availablemodelscircuit._

object OperationRunnerCommunication extends CommunicationAPI.Communicator[ModelAction] {
  val responseTopic: String = APIOperationRunner.topicResponse

  def onReceiveMessage(message: SPMessage): Unit = {
    import APIOperationRunner.{Runner, Runners, StateEvent}

    val response = message.oneOf[StateEvent].or[Runner].or[Runners]
    val state = VDCircuit.readModelState

    for ((header, body) <- response.get) {
      body match {
        case StateEvent(runnerID, runnerState, runInAuto, disableConditionGroups) =>

        case Runner(setup) =>

        case Runners(setups) =>

      }
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
}
