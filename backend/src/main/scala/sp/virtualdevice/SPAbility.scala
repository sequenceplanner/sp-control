//package sp.virtualdevice
//
//import sp.domain._
//import Logic._
//
//import VirtualDevice._
//
//trait SPAbility {
//
//}
//
//
//final case class Ability(name: String,
//                         id: ID = ID.newID,
//                         preCondition: Condition = Condition(AlwaysFalse, List()),
//                         started: Condition = Condition(AlwaysTrue, List()),
//                         postCondition: Condition = Condition(AlwaysTrue, List()),
//                         resetCondition: Condition = Condition(AlwaysTrue, List()),
//                         parameterIDs: List[ID] = List(),
//                         resultIDs: List[ID] = List(),
//                         attributes: SPAttributes = SPAttributes())
//
//
//
//trait AbilityActorLogic extends AbilityLogic {
//
//  import sp.AbilityStatus._
//
//  val ability: Ability
//  lazy val ids = idsFromAbility(ability)
//
//  var state: String = UnavailableTag
//  var count: Long = 0
//  var currentCaller = SPAttributes()
//
//
//  def makeUnavailable() = state = UnavailableTag
//  def makeAvailable() = state = NotEnabled
//
//  def start(s: Map[ID, SPValue]): Option[Map[ID, SPValue]] = {
//    val tH = evalState(s, "start")
//    if (state == StartingTag || state == ExecutingTag){
//      Some(tH._2)
//    } else None
//  }
//
//  def reset(s: Map[ID, SPValue]) = {
//    val tH = evalState(s, "reset")
//    tH._2
//  }
//
//
//  /**
//    * Evaluates the ability state and updates it if needed
//    * @param currentState Current state
//    * @param cmd to start, reset or fail the ability, Leave blank if no cmd
//    * @return
//    */
//  def evalState(currentState: Map[ID, SPValue], cmd: String = ""): (Option[String], Map[ID, SPValue]) = {
//    val theState = SPState(state = currentState)
//    val abilityState = updateState(theState, cmd)
//
//    val newAState = if (abilityState._1.isEmpty) None else Some(abilityState._1)
//    val newRState = if (theState != abilityState._2) Some(abilityState._2.state) else None
//
//    newAState.foreach(x => state = x)
//    (newAState, abilityState._2.state)
//  }
//
//  /**
//    * The main state machine, taking the state and a command (start or reset) and updated the ability state
//    * To have a synced execution, i.e. that there is a well defined state in the real system that
//    * defined that the ability is execution, set the "syncedExecution" attributes to true in
//    * the abilities attributes.
//    * Set the "syncedFinished" attributes to true if there is a well defined finshed state
//    * @param theState Current state
//    * @param cmd Either "start" or "reset" (and fail soon)
//    * @return (String, SPState), Where the string states the new state, or empty if
//    *         the state was not changed. And the state is the new updated state
//    */
//  def updateState(theState: SPState, cmd: String): (String, SPState) = state match {
//    case x if cmd == "start" &&
//      (x == EnabledTag || x == NotEnabledTag || x == UnavailableTag) &&
//      ability.preCondition.eval(theState) =>
//
//      if (ability.started.eval(theState)) // skipping starting for simple abilities
//        (Executing, ability.started.next(ability.preCondition.next(theState)))
//      else
//        (Starting, ability.preCondition.next(theState))
//
//    case x if cmd == "reset" && !(x == ForcedResetTag || x == UnavailableTag || x == EnabledTag || x == NotEnabledTag) =>
//      (ForcedReset, theState)
//
//    case x if x == ForcedResetTag =>
//      val updS =  ability.resetCondition.next(theState)
//      (checkEnabled(updS), updS)
//
//    case x if x == UnavailableTag && theState.state.nonEmpty =>
//      (NotEnabled, theState)
//
//    case x if x != ExecutingTag && syncedExecution(ability) && ability.started.eval(theState) =>
//      (Executing, theState)
//    case x if x != FinishedTag && syncedFinished(ability) && ability.postCondition.eval(theState) =>
//      (Finished, theState)
//
//    case x if x == NotEnabledTag && ability.preCondition.eval(theState) =>
//      (Enabled, theState)
//    case x if x == EnabledTag && !ability.preCondition.eval(theState) =>
//      (NotEnabled, theState)
//
//
//    case x if x == StartingTag && ability.started.eval(theState) =>
//      (Executing, ability.started.next(theState))
//    case x if x == ExecutingTag && ability.postCondition.eval(theState) =>
//      (Finished, ability.postCondition.next(theState))
//    case x if x == FinishedTag && ability.resetCondition.eval(theState) =>
//      val updS = ability.resetCondition.next(theState)
//      (checkEnabled(updS), updS)
//
//
//    //case x if x == failed => // not yet used. fail if driver fails
//
//    case _ => ("", theState) // No change
//
//  }
//
//  def checkEnabled(tS: SPState): String = if (ability.preCondition.eval(tS)) Enabled else NotEnabled
//
//
//
//  def createNotStartingErrorMessage(): String = {
//    s"state: $state"
//  }
//
//}
//
//trait AbilityLogic {
//  /**
//    *
//    * @return IDS of all relevant values of and within an ability.
//    */
//  def idsFromAbility(ability: APIAbilityHandler.Ability): Set[ID] = {
//    val conditionIDs = Set(
//      ability.preCondition,
//      ability.postCondition,
//      ability.started,
//      ability.resetCondition
//    ).flatMap(extractVariableIDs)
//
//    conditionIDs ++ ability.parameterIDs ++ ability.resultIDs
//  }
//
//  def extractVariableIDs(p: Condition): List[ID] = {
//    idsFromGuard(p.guard) ++ idsFromAction(p.action)
//  }
//
//  /**
//    * @return All IDs present in the guard clause.
//    */
//  def idsFromGuard(guard: Proposition): List[ID] = {
//    guard match {
//      case AND(xs) => xs.flatMap(idsFromGuard)
//      case OR(xs) => xs.flatMap(idsFromGuard)
//      case NOT(x) => idsFromGuard(x)
//      case pe: PropositionEvaluator =>
//        val xs = List(pe.left, pe.right)
//        xs.collect{
//          case SVIDEval(id) => id
//
//        }
//      case x => List()
//    }
//  }
//
//  /**
//    *
//    * @return The IDs of all actions and the ID of their respective values where applicable.
//    */
//  def idsFromAction(actions: List[Action]): List[ID] = {
//    actions.flatMap { action =>
//      val actionId = List(action.id)
//
//      action.value match {
//        case ASSIGN(id) => id :: actionId
//        case _ => actionId
//      }
//    }
//  }
//
//  /**
//    * Check the attributes if this ability has a well defined executing
//    * state. If so, it should jump to it if the starting guard is true
//    * @param a
//    */
//  def syncedExecution(a: APIAbilityHandler.Ability): Boolean = {
//    a.attributes.getAs[Boolean]("syncedExecution").getOrElse(false)
//  }
//
//  /**
//    * Check the attributes if this ability has a well defined finished
//    * state. If so, it should jump to it if the postcondition guard is true
//    * @param a
//    */
//  def syncedFinished(a: APIAbilityHandler.Ability): Boolean = {
//    a.attributes.getAs[Boolean]("syncedFinished").getOrElse(false)
//  }
