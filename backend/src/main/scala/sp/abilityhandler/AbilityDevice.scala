package sp.abilityhandler

import sp.domain._
import sp.domain.Logic._
import akka.actor._

// TODO: 180311: Currently we do not have a synchronized state in the
// TODO: abilities. If guaranteed booking is needed, it must be done on the
// TODO: operation layer


/**
  * Probably merge with the VD later...
  */
class AbilityHandlerMaker extends Actor
  with ActorLogging
  with AbilityLogic
  with sp.service.ServiceCommunicationSupport
  with sp.service.MessageBussSupport {

  import context.dispatcher
  subscribe(APIAbilityHandler.topicRequest)

  // Setting up the status response that is used for identifying the service in the cluster
  val statusResponse = AbilityHandler.attributes.copy(
    instanceName = "AbilityHandlerMaker"
  )

  var ahs: Map[ID, ActorRef] = Map()

  override def receive = {
    //case x if {log.debug(s"ability handler maker got: $x"); false} => false
    case x: String =>
      for {
        m <- SPMessage.fromJson(x)
        h <- m.getHeaderAs[SPHeader] // add filter here if we want multiple makers
        b <- m.getBodyAs[APIAbilityHandler.Request]
      } yield {
        val updH = h.swapToAndFrom()
        b match {
          case setup : APIAbilityHandler.SetUpAbilityHandler =>
            log.debug("Setting up an ability handler")
            log.debug(setup.toString)
            if (ahs.contains(setup.id)){
              publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(updH, APISP.SPError(s"Abilityhandler with id ${setup.id} already exist")))
            } else {
              val a = context.actorOf(AbilityHandler.propsHandler(setup.name, setup.id, setup.vd))
              ahs += setup.id -> a
              context.watch(a)
              a ! APIAbilityHandler.SetUpAbilities(setup.abilities, setup.handshake) // no need for jsonify since this is also matched in AbilityHandler
              publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(updH, APISP.SPDone()))
            }
          case APIAbilityHandler.TerminateAllAbilities =>
            ahs.foreach(a =>  { a._2 ! PoisonPill})
            publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(updH, APISP.SPDone()))
          case _=>
        }
      }

    case Terminated(x) => ahs = ahs.filterNot(_._2 == x)
      if (ahs.isEmpty) publish(APIAbilityHandler.topicResponse, SPMessage.makeJson(SPHeader(from = APIAbilityHandler.service), APIAbilityHandler.AbilitiesTerminated))
    case other =>
  }

}

// Internal api between abilities and handler
case class StartAbility(state: Map[ID, SPValue], reqID: ID, params: Map[ID, SPValue], attributes: SPAttributes = SPAttributes())
case class ResetAbility(state: Map[ID, SPValue])
case object GetIds
case class NewState(s: Map[ID, SPValue])
case object UnAvailable
case object GetState

case class AbilityIds(ability: ID, ids: Set[ID])
case class CanNotStart(reqID: ID, ability: ID, error: String)
case class AbilityStateChange(ability: ID, state: String, count: Long, requestID: Option[ID])
case class StateIsMissingIDs(ability: ID, xs: Set[ID])
case class StateUpdReq(ability: ID, state: Map[ID, SPValue])


// TODO: Merge the state of the abilities into the VD-state (when the VD is moved)
trait AbilityActorLogic extends AbilityLogic{
  val ability: APIAbilityHandler.Ability
  lazy val ids = idsFromAbility(ability)

  import AbilityStatus._

  var state: String = Unavailable
  var count: Long = 0
  var currentCaller = SPAttributes()


  def makeUnavailable() = state = Unavailable
  def makeAvailable() = state = NotEnabled

  def start(s: Map[ID, SPValue]): Option[Map[ID, SPValue]] = {
    val tH = evalState(s, "start")
    if (state == Starting || state == Executing){
      Some(tH._2)
    } else None
  }

  def reset(s: Map[ID, SPValue]) = {
    val tH = evalState(s, "reset")
    tH._2
  }


  /**
    * Evaluates the ability state and updates it if needed
    * @param currentState Current state
    * @param cmd to start, reset or fail the ability, Leave blank if no cmd
    * @return
    */
  def evalState(currentState: Map[ID, SPValue], cmd: String = ""): (Option[String], Map[ID, SPValue]) = {
    val theState = SPState(state = currentState)
    val abilityState = updateState(theState, cmd)

    val newAState = if (abilityState._1.isEmpty) None else Some(abilityState._1)
    val newRState = if (theState != abilityState._2) Some(abilityState._2.state) else None

    newAState.foreach(x => state = x)
    (newAState, abilityState._2.state)
  }

  /**
    * The main state machine, taking the state and a command (start or reset) and updated the ability state
    * To have a synced execution, i.e. that there is a well defined state in the real system that
    * defined that the ability is execution, set the "syncedExecution" attributes to true in
    * the abilities attributes.
    * Set the "syncedFinished" attributes to true if there is a well defined finshed state
    * @param theState Current state
    * @param cmd Either "start" or "reset" (and fail soon)
    * @return (String, SPState), Where the string states the new state, or empty if
    *         the state was not changed. And the state is the new updated state
    */
  def updateState(theState: SPState, cmd: String): (String, SPState) = state match {
    case x if cmd == "start" &&
      (x == Enabled || x == NotEnabled || x == Unavailable) &&
      ability.preCondition.eval(theState) =>

      if (ability.started.eval(theState)) // skipping starting for simple abilities
        (Executing, ability.started.next(ability.preCondition.next(theState)))
      else
        (Starting, ability.preCondition.next(theState))

    case x if cmd == "reset" && !(x == ForcedReset || x == Unavailable || x == Enabled || x == NotEnabled) =>
      (ForcedReset, theState)

    case x if x == ForcedReset =>
      val updS =  ability.resetCondition.next(theState)
      (checkEnabled(updS), updS)

    case x if x == Unavailable && theState.state.nonEmpty =>
      (NotEnabled, theState)

    case x if x != Executing && syncedExecution(ability) && ability.started.eval(theState) =>
      (Executing, theState)
    case x if x != Finished && syncedFinished(ability) && ability.postCondition.eval(theState) =>
      (Finished, theState)

    case x if x == NotEnabled && ability.preCondition.eval(theState) =>
      (Enabled, theState)
    case x if x == Enabled && !ability.preCondition.eval(theState) =>
      (NotEnabled, theState)


    case x if x == Starting && ability.started.eval(theState) =>
      (Executing, ability.started.next(theState))
    case x if x == Executing && ability.postCondition.eval(theState) =>
      (Finished, ability.postCondition.next(theState))
    case x if x == Finished && ability.resetCondition.eval(theState) =>
      val updS = ability.resetCondition.next(theState)
      (checkEnabled(updS), updS)


    //case x if x == failed => // not yet used. fail if driver fails

    case _ => ("", theState) // No change

  }

  def checkEnabled(tS: SPState): String = if (ability.preCondition.eval(tS)) Enabled else NotEnabled



  def createNotStartingErrorMessage(): String = {
    s"state: $state"
  }

}

trait AbilityLogic {
  /**
    *
    * @return IDS of all relevant values of and within an ability.
    */
  def idsFromAbility(ability: APIAbilityHandler.Ability): Set[ID] = {
    val conditionIDs = Set(
      ability.preCondition,
      ability.postCondition,
      ability.started,
      ability.resetCondition
    ).flatMap(extractVariableIDs)

    conditionIDs ++ ability.parameterIDs ++ ability.resultIDs
  }

  def extractVariableIDs(p: Condition): List[ID] = {
    idsFromGuard(p.guard) ++ idsFromAction(p.action)
  }

  /**
    * @return All IDs present in the guard clause.
    */
  def idsFromGuard(guard: Proposition): List[ID] = {
    guard match {
      case AND(xs) => xs.flatMap(idsFromGuard)
      case OR(xs) => xs.flatMap(idsFromGuard)
      case NOT(x) => idsFromGuard(x)
      case pe: PropositionEvaluator =>
        val xs = List(pe.left, pe.right)
        xs.collect{
          case SVIDEval(id) => id

        }
      case x => List()
    }
  }

  /**
    *
    * @return The IDs of all actions and the ID of their respective values where applicable.
    */
  def idsFromAction(actions: List[Action]): List[ID] = {
    actions.flatMap { action =>
      val actionId = List(action.id)

      action.value match {
        case ASSIGN(id) => id :: actionId
        case _ => actionId
      }
    }
  }

  /**
    * Check the attributes if this ability has a well defined executing
    * state. If so, it should jump to it if the starting guard is true
    * @param a
    */
  def syncedExecution(a: APIAbilityHandler.Ability): Boolean = {
    a.attributes.getAs[Boolean]("syncedExecution").getOrElse(false)
  }

  /**
    * Check the attributes if this ability has a well defined finished
    * state. If so, it should jump to it if the postcondition guard is true
    * @param a
    */
  def syncedFinished(a: APIAbilityHandler.Ability): Boolean = {
    a.attributes.getAs[Boolean]("syncedFinished").getOrElse(false)
  }
}
