package spgui.availablemodelscircuit

import diode.ActionResult.ModelUpdate
import diode.{Action, Circuit, ModelR, ModelRO}
import diode.react.ReactConnector
import monocle.macros.{GenLens, Lenses}
import sp.domain
import sp.domain.ID
import sp.models.APIModel.{ModelHistory, ModelInformation}
import spgui.SimpleSet

case class StateValueHolder[A](prevValue: Option[A], currentValue: A) {
  def set(value: A): StateValueHolder[A] = if (currentValue != value) StateValueHolder(Some(currentValue), value) else this
}

// TODO This assumes that the "id" value on ModelInformation and ModelHistory is equal to the model ID that
// TODO is associated with the information & history. If the assumption is correct, this comment can be removed.
// TODO (Jonathan Krän, 6/14/2018)
@Lenses case class ModelMock(id: domain.ID, info: Option[ModelInformation] = None, history: Option[ModelHistory] = None) {
  import MergeUtility.MergeModelInformation

  def merge(that: ModelMock): ModelMock = {
    val infoInit: Option[ModelInformation] = None

    ModelMock(
      id,
      List(info, that.info).flatten.foldLeft(infoInit)((acc, next) => acc.map(_ merge next)),
      that.history
    )
  }
}

@Lenses case class ModelsCircuitState(models: SimpleSet[ID, ModelMock], activeModelId: Option[ID], previousActiveModelId: Option[ID]) {
  def activeModel: Option[ModelMock] = activeModelId.map(models.apply)
}

sealed trait ModelAction extends Action
case class RemoveModel(modelId: ID) extends ModelAction
case class SaveModel(model: ModelMock) extends ModelAction
case class UpdateModel(model: ModelMock) extends ModelAction
case class SetActiveModel(modelId: ID) extends ModelAction
case class SetActiveModel(modelId: ID) extends ModelAction
case class AddMockModels(models: Iterable[ModelMock]) extends ModelAction
object AddMockModels {
  def apply(models: ModelMock*) = new AddMockModels(models)
}

object ModelsCircuit extends Circuit[ModelsCircuitState] with ReactConnector[ModelsCircuitState] {
  import ModelsCircuitState.models

  type StateFn = ModelsCircuitState => ModelsCircuitState

  def readState: ModelR[ModelsCircuitState, ModelsCircuitState] = zoom(identity)

  override protected def initialModel: ModelsCircuitState = ModelsCircuitState(SimpleSet(_.id), None, None)

  override protected def actionHandler: ModelsCircuit.HandlerFunction = (state, action) => {
    val f = handleByLens(action)

    Some(ModelUpdate(f(state)))
  }

  private def updateActiveId(modelId: ID): StateFn = state => {
    if (state.activeModelId.contains(modelId))
      state
    else
      state.copy(previousActiveModelId = state.activeModelId, activeModelId = Some(modelId))
  }

  private def removeModel(modelId: ID): StateFn = models.modify(_.removeByKey(modelId)) andThen (s => {
    if (s.activeModelId.contains(modelId))
      s.copy(previousActiveModelId = s.activeModelId, activeModelId = None)
    else
      s
  })

  def handleByLens(action: Any): StateFn = action match {
    case SaveModel(model) => models.modify(_ + model)

    case RemoveModel(modelId) =>
      models.modify(ms => ms.removeByKey(modelId)) andThen removeModel(modelId)

    case UpdateModel(model) =>
      models.modify(_.replace(model))

    case SetActiveModel(modelId) =>
      updateActiveId(modelId)

    case AddMockModels(newModels) =>
      models.modify { MergeUtility.mergeModelIterables(_, newModels) }

    case unknownAction =>
      println(s"[ModelsCircuit] Got unknown action: $unknownAction")
      s => s
  }
}

object MergeUtility {
  def mergeModelIterables(models: SimpleSet[ID, ModelMock], newModels: Iterable[ModelMock]): SimpleSet[ID, ModelMock] = {
    newModels.foldLeft(models) { case (acc, next) =>
      if (acc.contains(next.id)) acc.modify(_ merge next)(next)
      else acc + next
    }
  }

  implicit class MergeModelInformation(info: ModelInformation) {
    /**
      * Merges two ModelInformation objects. Uses otherInfo as source of truth where necessary.
      */
    def merge(otherInfo: ModelInformation) = ModelInformation(
      name = otherInfo.name,
      id = otherInfo.id,
      version = otherInfo.version max info.version,
      noOfItems = otherInfo.noOfItems,
      attributes = otherInfo.attributes
    )
  }
}