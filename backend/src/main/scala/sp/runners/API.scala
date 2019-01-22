package sp.runners

import sp.domain._
import Logic._
import akka.stream._
import akka.stream.scaladsl._
import akka.NotUsed

import Shared._

object API {
  // TODO: serializable flows
  // case class ResourcePipeline(message: Struct, flowDef: SPAttributes)
  case class SPResource(
    resource: ID,
    initialState: State,
    outputs: List[Sink[State, _]],
    inputs: List[Source[State, _]]
  )
  case class SPRunner(
    operations: List[Operation],
    initialState: State,
    stateVariables: Struct,
    transitionSystem: List[sp.runners.RunnerLogic.OperationTransition]
  )

  case class SetupRunnerInstance(
    id: ID,
    items: List[IDAble],
    resources: List[SPResource],
    runner: SPRunner
  )
}

object AbilityRunnerTransitions {
  import RunnerLogic._

  // states
  object AbilityStates {
    val notEnabled = "notEnabled"
    val enabled = "enabled"
    val starting = "starting"
    val executing = "executing"
    val finished = "finished"
  }

  // kinds
  object AbilityKinds {
    val pre = "pre"
    val started = "started"
    val post = "post"
    val postAlternative = "postAlternative"
    val reset = "reset"
  }

  object AbilityTransitions {
    import AbilityStates._
    import AbilityKinds._

    val isEnabled = OperationTransition(
      states = Set(notEnabled, finished),
      conditionKind = Set(pre),
      nextState = enabled,
      event = None,
      alwaysTrueIfNoConditions = true,
      enableAlternatives = false,
      onlyGuard = true,
      negateGuard = false
    )
    val enabledToNotEnabled = OperationTransition(
      states = Set(enabled), // abilities will be finished until they are enabled again.
      conditionKind = Set(pre),
      nextState = notEnabled,
      event = None,
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = true,
      negateGuard = true // this will go back if pre guard is false when in enabled
    )
    val enabledToStarting = OperationTransition(
      states = Set(enabled),
      conditionKind = Set(pre),
      nextState = starting,
      event = Some("start"),
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = false,
      negateGuard = false
    )
    val startingToExec = OperationTransition(
      states = Set(starting),
      conditionKind = Set(started),
      nextState = executing,
      event = None,
      alwaysTrueIfNoConditions = true,
      enableAlternatives = false,
      onlyGuard = false,
      negateGuard = false
    )
    val syncedExecution = OperationTransition( // For operations that should sync with reality
      states = Set(notEnabled, enabled, starting, finished),
      conditionKind = Set("isExecuting"),
      nextState = executing,
      event = None,
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = true,
      negateGuard = false
    )
    val syncedFinished = OperationTransition( // For operations that should sync with reality
      states = Set(notEnabled, enabled, starting, executing),
      conditionKind = Set("isFinished"),
      nextState = finished,
      event = None,
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = false,  /// this will set the actions continously!!
      negateGuard = false
    )

  }

  val abilityTransitionSystem = List(
    AbilityTransitions.isEnabled,
    AbilityTransitions.enabledToNotEnabled,
    AbilityTransitions.enabledToStarting,
    AbilityTransitions.startingToExec,
    AbilityTransitions.syncedExecution,
    AbilityTransitions.syncedFinished
  )
}


// Inte testad! Måste skriva test för denna så att det fungerar som tänkt
object OriginalAbilityRunnerTransitions {
  import RunnerLogic._

  // states
  object AbilityStates {
    val notEnabled = "notEnabled"
    val enabled = "enabled"
    val starting = "starting"
    val executing = "executing"
    val finished = "finished"
  }

  // kinds
  object AbilityKinds {
    val pre = "pre"
    val started = "started"
    val post = "post"
    val postAlternative = "postAlternative"
    val reset = "reset"
  }

  object AbilityTransitions {
    import AbilityStates._
    import AbilityKinds._

    val notEnabledToEnabled = OperationTransition(
      states = Set(notEnabled),
      conditionKind = Set(pre),
      nextState = enabled,
      event = None,
      alwaysTrueIfNoConditions = true,
      enableAlternatives = false,
      onlyGuard = true,
      negateGuard = false
    )
    val enabledToNotEnabled = OperationTransition(
      states = Set(enabled),
      conditionKind = Set(pre),
      nextState = notEnabled,
      event = None,
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = true,
      negateGuard = true // this will go back if pre guard is false when in enabled
    )
    val enabledToStarting = OperationTransition(
      states = Set(enabled),
      conditionKind = Set(pre),
      nextState = starting,
      event = Some("start"),
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = false,
      negateGuard = false
    )
    val startingToExec = OperationTransition(
      states = Set(starting),
      conditionKind = Set(started),
      nextState = executing,
      event = None,
      alwaysTrueIfNoConditions = true,
      enableAlternatives = false,
      onlyGuard = false,
      negateGuard = false
    )

    val execToFinished = OperationTransition(
      states = Set(executing),
      conditionKind = Set(post),
      nextState = finished,
      event = None,
      alwaysTrueIfNoConditions = true,
      enableAlternatives = false,
      onlyGuard = false,
      negateGuard = false
    )
    val execToFinishedAlt = OperationTransition(
      states = Set(executing),
      conditionKind = Set(postAlternative),
      nextState = finished,
      event = None,
      alwaysTrueIfNoConditions = false,
      enableAlternatives = true,
      onlyGuard = false,
      negateGuard = false
    )
    val finToNotEnabled = OperationTransition(
      states = Set(finished),
      conditionKind = Set(reset),
      nextState = notEnabled,
      event = Some("reset"),
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = false,
      negateGuard = false
    )
    val forceReset = OperationTransition(
      states = Set(starting, executing, finished),
      conditionKind = Set("ShouldNotHaveAnyConditions"),
      nextState = notEnabled,
      event = Some("forceReset"),
      alwaysTrueIfNoConditions = true,
      enableAlternatives = false,
      onlyGuard = false,
      negateGuard = false
    )
    val syncedExecution = OperationTransition( // For operations that should sync with reality
      states = Set(notEnabled, enabled, starting, finished),
      conditionKind = Set("isExecuting"),
      nextState = executing,
      event = None,
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = true,
      negateGuard = false
    )
    val syncedFinished = OperationTransition( // For operations that should sync with reality
      states = Set(notEnabled, enabled, starting, executing),
      conditionKind = Set("isFinished"),
      nextState = finished,
      event = None,
      alwaysTrueIfNoConditions = false,
      enableAlternatives = false,
      onlyGuard = true,
      negateGuard = false
    )

  }

  val abilityTransitionSystem = List(
    AbilityTransitions.notEnabledToEnabled,
    AbilityTransitions.enabledToNotEnabled,
    AbilityTransitions.enabledToStarting,
    AbilityTransitions.startingToExec,
    AbilityTransitions.execToFinished,
    AbilityTransitions.execToFinishedAlt,
    AbilityTransitions.finToNotEnabled,
    AbilityTransitions.forceReset,
    AbilityTransitions.syncedExecution,
    AbilityTransitions.syncedFinished
  )
}
