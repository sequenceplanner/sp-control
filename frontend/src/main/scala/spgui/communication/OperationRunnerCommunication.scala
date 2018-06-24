package spgui.communication

import sp.abilityhandler.APIAbilityHandler.GetAbility
import sp.domain.SPMessage
import sp.runners.APIOperationRunner
import sp.runners.APIOperationRunner.Setup
import spgui.SPMessageUtil.BetterSPMessage
import spgui.circuits.main.handlers.{CreateRunner, UpdateRunners, RunnerAction, UpdateRunner}
import spgui.circuits.main.FrontendState
import spgui.circuits.main.handlers.Aliases.{AbilityId, OperationId}

object OperationRunnerCommunication extends CommunicationAPI.Communicator[String, RunnerAction] {
  import sp.runners.{APIOperationRunner => API}

  import API.Response.fOperationRunnerResponse

  def onReceiveMessage(message: SPMessage): Unit = {
    val response = message.as[API.Response]

    for ((_, body) <- response) body match {
        case API.StateEvent(runnerID, operationStates, runInAuto, disableConditionGroups) =>
          localDispatch(UpdateRunner(runnerID, operationStates, runInAuto, disableConditionGroups))

        case API.Runner(setup) =>
          val associations = getAssociations(setup)
          // Request information about the abilities
          associations.foreach { case(_, abilityId) => AbilityCommunication.postRequest(GetAbility(abilityId)) }

          localDispatch(CreateRunner(setup, associations))

        case API.Runners(setups) =>
        localDispatch(UpdateRunners(setups))
    }
  }

  def getAssociations(setup: Setup): Map[OperationId, AbilityId] = {
      val data = for {
        operation <- setup.ops
        abilityId <- setup.opAbilityMap.get(operation.id)
      } yield operation.id -> abilityId

      data.toMap
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
