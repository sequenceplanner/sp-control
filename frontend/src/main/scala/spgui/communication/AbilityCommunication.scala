package spgui.communication

import sp.abilityhandler.APIAbilityHandler
import sp.domain.SPMessage
import spgui.SPMessageUtil.BetterSPMessage
import spgui.availablemodelscircuit._

object AbilityCommunication extends CommunicationAPI.Communicator[AbilityHandlerState, AbilityAction] {
  import sp.abilityhandler.{APIAbilityHandler => API}

  def onReceiveMessage(message: SPMessage): Unit = {


    val response = message.as[API.Response]
    for ((header, body) <- response) body match {
      case API.AbilitiesTerminated =>

      case API.TheAbility(ability) =>

      case API.CmdID(commandId) =>

      case API.AbilityStarted(abilityId) =>

      case API.AbilityCompleted(abilityId, result) =>

      case API.AbilityState(abilityId, state) =>

      case API.Abilities(abilities) =>

      case API.AbilitiesByIdAndName(abilityData) =>
    }
  }

  def postRequest(request: API.Request): Unit = {
    post(
      request,
      from = "VDTrackerWidget",
      to = API.service,
      topic = API.topicRequest
    )
  }

  override protected def stateAccessFunction: FrontendState => AbilityHandlerState = _.abilityState
  val responseTopic: String = APIAbilityHandler.topicResponse
}
