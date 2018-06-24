package spgui.circuits.main.handlers

import diode.{Action, ModelRW}
import monocle.macros.Lenses
import sp.domain._
import sp.runners.APIOperationRunner.Setup
import spgui.SimpleSet
import spgui.circuits.main.handlers.Aliases._
import spgui.communication.OperationRunnerCommunication

trait RunnerAction extends Action
case class UpdateRunner(runnerId: ID, operationStates: Map[OperationId, SPValue], runInAuto: Boolean, disabledGroups: Set[SPValue]) extends RunnerAction
case class CreateRunner(setup: Setup, associations: Map[OperationId, AbilityId]) extends RunnerAction
case class UpdateRunners(setups: List[Setup]) extends RunnerAction

object RunnerHandler {
  val initialState: RunnerHandlerState = RunnerHandlerState(new SimpleSet(_.id, Map()))
}

@Lenses case class RunnerHandlerState(runners: SimpleSet[ID, Runner])


class RunnerHandler[M](modelRW: ModelRW[M, RunnerHandlerState]) extends StateHandler[M, RunnerHandlerState, RunnerAction](modelRW) {
  import RunnerHandlerState.runners

  val identityState: StateFn = identity


  override def onAction: PartialFunction[RunnerAction, Reaction] = {
    case update @ UpdateRunner(runnerId, operationStates, _, _) => react {
      runners.modify(_.modifyByKey { runner =>

        val relevantOperations = operationStates.flatMap { case (k, v) =>
          runner.operations.get(k).map(operation => (k, v, operation))
        }

        val newOperations = relevantOperations.foldLeft(runner.operations) { case (acc, (k, v, operation)) =>
          acc + operation.copy(state = Map(k -> v))
        }

        runner.copy(
          operations = newOperations,
          runInAuto = update.runInAuto,
          disabledGroups = update.disabledGroups
        )
      }(runnerId))
    }

    case CreateRunner(setup, associations) =>
      runners.modify(_ + Runner.fromSetup(setup, associations))

    case UpdateRunners(setups) =>
      setups
        .map(setup => runners.modify(_ + Runner.fromSetup(setup, OperationRunnerCommunication.getAssociations(setup))))
        .foldLeft(identityState)(_ compose _)
      .andThen(runners.modify(_.filterKeys( key => setups.exists(_.runnerID == key)))) // Remove old runners
  }

  override def acceptAction: Action => Boolean = {
    case _: RunnerAction => true
    case _ => false
  }

}

case class OperationData(operation: Operation, state: Map[ID, SPValue] = Map()) {
  import sp.domain.logic.AttributeLogic._

  val name: String = operation.name
  val conditions: List[Condition] = operation.conditions
  val attributes: SPAttributes = operation.attributes
  val id: OperationId = operation.id

  def preConditions: Iterable[Condition] = {
    conditions.filter(c => c.attributes.getAs[String]("kind").contains("pre"))
  }

  def postConditions: Iterable[Condition] = {
    conditions.filter(c => c.attributes.getAs[String]("kind").contains("post"))
  }
}

// TODO Are disabled groups necessary? They seem to not be used anywhere, at least not in the frontend
// TODO Are all these different maps really necessary? Structure should probably be looked over
case class Runner private (
                            id: ID,
                            operations: SimpleSet[ID, OperationData] = new SimpleSet(_.id, Map()),
                            disabledGroups: Set[SPValue] = Set(),
                            initialState: Map[ID, SPValue] = Map(), // What ID?
                            variables: Map[OperationModelId, VDModelId] = Map(),
                            associations: Map[OperationId, AbilityId] = Map(),
                            abilityParameters: Map[AbilityId, Set[OperationModelId]] = Map(),
                            runInAuto: Boolean = false // TODO Not sure if false is desired default value
                          ) {
  /**
    * Set an association between an operation and an ability.
    */
  def associate(operationId: OperationId, abilityId: AbilityId): Runner = {
    copy(associations = associations + (operationId -> abilityId))
  }
}

object Runner {
  import sp.runners.{APIOperationRunner => API}
  def fromSetup(setup: API.Setup, associations: Map[OperationId, AbilityId]): Runner = {
    val runner = Runner(setup.runnerID)
    runner.copy(
      operations = runner.operations.addAll(setup.ops.map(OperationData(_))),
      initialState = setup.initialState,
      variables = setup.variableMap,
      associations = associations,
      abilityParameters = setup.abilityParameters
    )
  }
}