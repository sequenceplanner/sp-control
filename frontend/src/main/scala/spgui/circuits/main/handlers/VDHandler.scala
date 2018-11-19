package spgui.circuits.main.handlers

import diode.{Action, ModelRW}
import monocle.macros.Lenses
import sp.domain.{ID, SPAttributes, SPValue}
import spgui.SimpleSet
import spgui.circuits.main.handlers.Aliases._

trait VDAction extends Action
case class UpdateRunnerState(runner: ID, state: Map[ID, SPValue]) extends VDAction
case class ModelNames(names: List[VDModelName]) extends VDAction
case class RunnerCreated(id: RunnerId) extends VDAction
case object TerminateAllVirtualDevices extends VDAction

@Lenses case class VDHandlerState( runnerStates: Map[ID, Map[ID, SPValue]],
                                   availableVDModels: List[VDModelName],
                                   latestActiveRunnerId: Option[RunnerId]
                                 )


// TODO Someone with domain knowledge needs to take a look at how updates happen.
// TODO It is probably incorrect in several places. For example, state might be
// TODO when it should actually be merged, etc.
class VDHandler[M](modelRW: ModelRW[M, VDHandlerState]) extends StateHandler[M, VDHandlerState, VDAction](modelRW) {
  import VDHandlerState.{runnerStates, availableVDModels, latestActiveRunnerId}

  override def onAction: PartialFunction[VDAction, Reaction] = {
    case UpdateRunnerState(id, state) =>
      react {
        runnerStates.set(value.runnerStates + (id -> state))
      }

    case ModelNames(names) =>
      availableVDModels.set(names)

    case RunnerCreated(id) =>
      latestActiveRunnerId.set(Some(id))


    // case TerminateAllRunners => react {
    //   runners.set(SimpleSet[RunnerId, Runner](_.id))
    // } globally {
    //   value.runners.map(_.id).foreach { runnerId =>
    //     OperationRunnerCommunication.postRequest(APIOperationRunner.TerminateRunner(runnerId))
    //   }


  }

  override def acceptAction: Action => Boolean = {
    case _: VDAction => true
    case _ => false
  }
}

object VDHandler {
  val initialState: VDHandlerState = VDHandlerState(Map(), List(), latestActiveRunnerId = None)
}
