package spgui.circuits.main.handlers

import diode.{Action, ModelRW}
import monocle.macros.Lenses
import sp.domain.{ID, SPAttributes, SPValue}
import spgui.SimpleSet

trait RunnerAction extends Action
case class AddRunner(runner: ID) extends RunnerAction
case class UpdateRunnerState(runner: ID, state: Map[ID, SPValue]) extends RunnerAction
case object TerminateAllRunners extends RunnerAction

// some mini model stuff here because lazy
case class ModelNames(names: List[String]) extends RunnerAction

@Lenses case class RunnerHandlerState(
  runnerStates: Map[ID, Map[ID, SPValue]],
  latestActiveRunnerId: Option[ID],
  availableMiniModels: List[String]
)


class RunnerHandler[M](modelRW: ModelRW[M, RunnerHandlerState]) extends StateHandler[M, RunnerHandlerState, RunnerAction](modelRW) {
  import RunnerHandlerState.{runnerStates, latestActiveRunnerId, availableMiniModels}

  override def onAction: PartialFunction[RunnerAction, Reaction] = {
    case AddRunner(id) =>
      latestActiveRunnerId.set(Some(id))

    case UpdateRunnerState(id, state) =>
      react {
        runnerStates.set(value.runnerStates + (id -> state))
      }

    case ModelNames(names) =>
      availableMiniModels.set(names)

    // case TerminateAllRunners => react {
    //   runners.set(SimpleSet[RunnerId, Runner](_.id))
    // } globally {
    //   value.runners.map(_.id).foreach { runnerId =>
    //     OperationRunnerCommunication.postRequest(APIOperationRunner.TerminateRunner(runnerId))
    //   }


  }

  override def acceptAction: Action => Boolean = {
    case _: RunnerAction => true
    case _ => false
  }
}

object RunnerHandler {
  val initialState: RunnerHandlerState = RunnerHandlerState(Map(), None, List())
}
