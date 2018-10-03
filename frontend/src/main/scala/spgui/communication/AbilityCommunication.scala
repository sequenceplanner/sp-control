// package spgui.communication

// import sp.domain.{ID, SPMessage, SPValue}
// import spgui.SPMessageUtil.BetterSPMessage
// import spgui.circuits.main.handlers._
// import spgui.circuits.main.FrontendState

// object AbilityCommunication extends CommunicationAPI.Communicator[VDHandlerState, AbilityAction] {
// //  import sp.abilityhandler.{APIAbilityHandler => API}

//   def onReceiveMessage(message: SPMessage): Unit = {


//     val response = message.as[API.Response]
//     for ((_, body) <- response) body match {
//       case API.AbilitiesTerminated =>

//       case API.TheAbility(ability) =>
//         ability.foreach(localDispatch _ compose AddAbility)

//       case API.CmdID(commandId) =>
//       // TODO What should happen upon receipt of this message?

//       case API.AbilityStarted(id) =>
//         localDispatch(UpdateAbility(id, _.copy(status = "Executing")))

//       case API.AbilityCompleted(id, result) =>
//         localDispatch(UpdateAbility(id, _.withResult(result)))

//       case API.AbilityState(id, state) =>
//         for ((newStatus, newCount) <- parseAbilityState(state))
//           localDispatch(UpdateAbility(id, _.copy(status = newStatus, count = newCount, state = state)))

//       case API.Abilities(abilities) =>
//         abilities.map(_.id).foreach(id => postRequest(GetAbility(id), reqId = Some(ID.newID)))

//         localDispatch(AddAbilities(abilities))

//       case API.AbilitiesByIdAndName(abilityData) =>
//         // TODO What should happen upon receipt of this message?
//     }
//   }

//   def postRequest(request: API.Request, reqId: Option[ID] = None, reply: String = defaultReply): Unit = {
//     post(
//       request,
//       from = "AbilityCommunication",
//       to = API.service,
//       topic = API.topicRequest,
//       reqId = reqId,
//       reply = reply
//     )
//   }

//   /**
//     * Parses ability state received as a [[scala.collection.immutable.Map[ID, SPValue]]].
//     * Assumes that there is only one pair in the map, that the key is irrelevant, and that
//     * the SPValue is of the form
//     * {{{
//     *   {
//     *     "state" -> String,
//     *     "counter" -> Int
//     *   }
//     * }}}
//     *
//     * @return A tuple of (abilityStatus, count).
//     */
//   def parseAbilityState(state: Map[ID, SPValue]): Option[(AbilityStatus, Int)] = {
//    val parsedState = state.map { case (_, json) =>
//       for {
//         status <- (json \ "state").asOpt[String].flatMap(AbilityStatus.fromString)
//         count <- (json \ "counter").asOpt[Int]
//       } yield (status, count)
//     }

//     parsedState.flatten.headOption
//   }

//   override protected def stateAccessFunction: FrontendState => VDHandlerState = _.virtualDevices
//   val responseTopic: String = APIAbilityHandler.topicResponse

//   override def defaultReply: String = "AbilityCommunication"
// }
