package sp.abilityhandler

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import sp.domain._
import sp.domain.Logic._


object AbilityActor {
  def props(ability: APIAbilityHandler.Ability) =
    Props(classOf[AbilityActor], ability)
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





class AbilityActor(val ability: APIAbilityHandler.Ability) extends Actor
  with AbilityActorLogic
  with ActorLogging
{
  var reqID: Option[ID] = None

  override def receive = {
    case GetIds => sender() ! AbilityIds(ability.id, ids)
    case UnAvailable =>
      makeUnavailable()
      sendAbilityState(sender())

    case x @ StartAbility(s, id, p, attr) =>
      log.debug("STARTING ABILITY")
      log.debug(x.toString)
      val res = start(s)
      res.collect {
        case updS if updS != s || p.nonEmpty =>
          reqID = Some(id)
          log.debug(updS.toString())
          if (p.nonEmpty) sender() ! StateUpdReq(ability.id, p)
          sender() ! StateUpdReq(ability.id, updS)
          sendAbilityState(sender())

          log.debug("")
          log.info("StartAbility v v v" + ability.name)
          log.debug("the state: " + s)
          log.debug("new ability state: " + state)
          log.debug("ability updated state: " + updS)
          log.debug("StartAbility END")
          log.debug("")
        case _ =>
          sender() ! CanNotStart(id, ability.id, createNotStartingErrorMessage())

        //fix timeout here if needed
      }

    //checkAndSend(res.getOrElse(s), sender())

    case ResetAbility(s) =>
      val res = reset(s)
      if (res != s)  sender() ! StateUpdReq(ability.id, res)
      sendAbilityState(sender())

    //checkAndSend(res.getOrElse(s), sender())

    case NewState(s) =>
      println("ABILITY GOT NEW STATE")
      val missingIDs = ids.diff(s.keySet)
      if (missingIDs.nonEmpty){
        sender() ! StateIsMissingIDs(ability.id, missingIDs)
      }

      // Always checking twice if the ability can jump multiple states
      // But limits too two to avoid free wheeling
      checkAndSend(s, sender())
      checkAndSend(s, sender())
    case GetState =>
      sendAbilityState(sender())
  }

  def sendAbilityState(to: ActorRef) = {
    to ! AbilityStateChange(ability.id, state, count, reqID)
  }

  def checkAndSend(s: Map[ID, SPValue], to: ActorRef): Unit = {
    val res = evalState(s)

    val changed = res._2 != s

    val print = res._1.isDefined || changed
    println("")
    println("<<<<<<<<<<<<<<<<<<<")
    println("checkAndSend v v v")
    println("the state: " + s)
    println("new ability state: " + res._1)
    println("ability updated state: " + res._2)
    println("checkAndSend END")
    println(">>>>>>>>>>>>>> >")
    println("")

    res._1.foreach { updS =>
      sendAbilityState(to)
    }
    if (changed)
      to ! StateUpdReq(ability.id, res._2)

  }


}



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
