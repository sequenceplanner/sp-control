package spgui.circuits.main.handlers

import diode.{Action, ModelRW}
import monocle.macros.Lenses
import sp.domain._
import sp.runners.APIOperationRunner
import sp.runners.APIOperationRunner.Setup
import spgui.SimpleSet
import spgui.circuits.main.handlers.Aliases._
import spgui.communication.OperationRunnerCommunication

trait RunnerAction extends Action
case class UpdateRunner(runnerId: ID, runnerState: Map[ID, SPValue], runInAuto: Boolean, disabledGroups: Set[SPValue]) extends RunnerAction
case class CreateRunner(setup: Setup, associations: Map[OperationId, AbilityId]) extends RunnerAction
case class UpdateRunners(setups: List[Setup]) extends RunnerAction
case object TerminateAllRunners extends RunnerAction

object RunnerHandler {
  val initialState: RunnerHandlerState = RunnerHandlerState(new SimpleSet(_.id, Map()))
}

@Lenses case class RunnerHandlerState(runners: SimpleSet[ID, Runner])

// TODO Someone with domain knowledge needs to take a look at how updates happen.
// TODO It is probably incorrect in several places. For example, state might be
// TODO when it should actually be merged, etc.
class RunnerHandler[M](modelRW: ModelRW[M, RunnerHandlerState]) extends StateHandler[M, RunnerHandlerState, RunnerAction](modelRW) {
  import RunnerHandlerState.runners

  val identityState: StateFn = identity


  override def onAction: PartialFunction[RunnerAction, Reaction] = {
    case update @ UpdateRunner(runnerId, state, _, _) => react {
      runners.modify(_.modifyByKey { runner =>

        runner.updateState(state).copy(
          runInAuto = update.runInAuto,
          disabledGroups = update.disabledGroups
        )
      }(runnerId))
    }

    case CreateRunner(setup, associations) =>
      runners.modify(_ + Runner.fromSetup(setup, associations))

    case UpdateRunners(setups) =>
      react {
        runners.modify { runners =>
          val newRunners = setups
            .filterNot(setup => runners.contains(setup.runnerID))
            .map(setup => Runner.fromSetup(setup, OperationRunnerCommunication.getAssociations(setup)))

          runners
            .filterKeys(id => setups.exists(_.runnerID == id)) // Remove old runners
            .addAll(newRunners)
        }
      }

    case TerminateAllRunners => react {
      runners.set(SimpleSet[RunnerId, Runner](_.id))
    } globally {
      value.runners.map(_.id).foreach { runnerId =>
        OperationRunnerCommunication.postRequest(APIOperationRunner.TerminateRunner(runnerId))
      }
    }
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
                            state: Map[ID, SPValue] = Map(),
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

  def updateState(state: Map[ID, SPValue]): Runner = {
    val relevantOperations = state.flatMap { case (k, v) =>
      operations.get(k).map(operation => (k, v, operation))
    }

    val newOperations = relevantOperations.foldLeft(operations) { case (acc, (k, v, operation)) =>
      acc + operation.copy(state = Map(k -> v))
    }

    copy(state = state, operations = newOperations)
  }
}

object Runner {
  import sp.runners.{APIOperationRunner => API}
  def fromSetup(setup: API.Setup, associations: Map[OperationId, AbilityId]): Runner = {
    val runner = Runner(setup.runnerID)
    runner.copy(
      operations = runner.operations.addAll(setup.ops.map(OperationData(_))),
      state = setup.initialState,
      variables = setup.variableMap,
      associations = associations,
      abilityParameters = setup.abilityParameters
    )
  }


}