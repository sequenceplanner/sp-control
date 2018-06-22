package spgui.communication

import sp.AbilityStatus
import sp.AbilityStatus.Executing
import sp.abilityhandler.APIAbilityHandler
import sp.domain.{ID, SPMessage, SPValue}
import spgui.SPMessageUtil.BetterSPMessage
import spgui.circuits.availablemodelscircuit._
import spgui.circuits.main.handlers._
import spgui.circuits.main.FrontendState

object AbilityCommunication extends CommunicationAPI.Communicator[AbilityHandlerState, AbilityAction] {
  import sp.abilityhandler.{APIAbilityHandler => API}

  def onReceiveMessage(message: SPMessage): Unit = {


    val response = message.as[API.Response]
    for ((_, body) <- response) body match {
      case API.AbilitiesTerminated =>

      case API.TheAbility(ability) =>
        ability.foreach(localDispatch _ compose AddAbility)

      case API.CmdID(commandId) =>
      // TODO What should happen upon receipt of this message?

      case API.AbilityStarted(id) =>
        localDispatch(UpdateAbility(id, _.copy(status = Executing)))

      case API.AbilityCompleted(id, result) =>
        localDispatch(UpdateAbility(id, _.withResult(result)))

      case API.AbilityState(id, state) =>
        for ((newStatus, newCount) <- parseAbilityState(state))
          localDispatch(UpdateAbility(id, _.copy(status = newStatus, count = newCount)))

      case API.Abilities(abilities) =>
        localDispatch(AddAbilities(abilities))

      case API.AbilitiesByIdAndName(abilityData) =>
        // TODO What should happen upon receipt of this message?
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

  /**
    * Parses ability state received as a [[scala.collection.immutable.Map[ID, SPValue]]].
    * Assumes that there is only one pair in the map, that the key is irrelevant, and that
    * the SPValue is of the form
    * {{{
    *   {
    *     "state" -> String,
    *     "counter" -> Int
    *   }
    * }}}
    *
    * @return A tuple of (abilityStatus, count).
    */
  def parseAbilityState(state: Map[ID, SPValue]): Option[(AbilityStatus, Int)] = {
   val parsedState = state.map { case (_, json) =>
      for {
        status <- (json \ "state").asOpt[String].flatMap(AbilityStatus.fromString)
        count <- (json \ "counter").asOpt[Int]
      } yield (status, count)
    }

    parsedState.flatten.headOption
  }

  override protected def stateAccessFunction: FrontendState => AbilityHandlerState = _.abilityState
  val responseTopic: String = APIAbilityHandler.topicResponse

  override def defaultReply: String = "AbilityCommunication"
}
