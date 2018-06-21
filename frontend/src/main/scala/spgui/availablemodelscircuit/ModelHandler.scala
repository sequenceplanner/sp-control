package spgui.availablemodelscircuit

import diode.{Action, ActionHandler, ActionResult, ModelRW}
import monocle.macros.Lenses
import sp.domain
import sp.domain.{ID, IDAble, SPAttributes}
import sp.models.APIModel.ModelInformation
import sp.models.{APIModel, APIModelMaker}
import spgui.{ModelCommunication, SimpleSet}

/**
  * Actions that can be sent to ModelHandler
  */
sealed trait ModelAction extends Action
case class RemoveModel(modelId: ID) extends ModelAction
case class SaveModel(model: ModelMock) extends ModelAction
case class UpdateModel(model: ModelMock) extends ModelAction
case class SetActiveModel(modelId: ID) extends ModelAction
case class SetItems(modelId: ID, items: List[IDAble]) extends ModelAction
case class AddMockModelIds(models: Iterable[ID]) extends ModelAction
object AddMockModelIds {
  def apply(modelIds: ID*) = new AddMockModelIds(modelIds)
}

@Lenses case class ModelMock(
                              id: domain.ID,
                              info: Option[ModelInformation] = None,
                              history: Option[List[(Int, SPAttributes)]] = None,
                              items: List[IDAble] = List()
                            )

@Lenses case class ModelsCircuitState(models: SimpleSet[ID, ModelMock], activeModelId: Option[ID], previousActiveModelId: Option[ID]) {
  def activeModel: Option[ModelMock] = activeModelId.map(models.apply)
}

object ModelHandler {
  val initialState = ModelsCircuitState(SimpleSet(_.id), None, None)
}

class ModelHandler[M](modelRW: ModelRW[M, ModelsCircuitState]) extends ActionHandler(modelRW) {
  import ModelsCircuitState.models

  type StateFn = ModelsCircuitState => ModelsCircuitState

  override protected def handle: PartialFunction[Any, ActionResult[M]] = {
    case action: ModelAction =>
        val f = handleAction(action)

        updated(f(value))
      }

  def handleAction(action: Any): StateFn = action match {
    case SaveModel(model) => models.modify(_ + model)

    case RemoveModel(modelId) =>
      ModelCommunication.Recipient.Model.postRequest(APIModelMaker.DeleteModel(modelId))
      removeModel(modelId)

    case UpdateModel(model) =>
      // TODO Reflect change in backend
      models.modify(_.replace(model))

    case SetActiveModel(modelId) =>
      updateActiveId(modelId)

    case SetItems(modelId, items) =>
      ModelCommunication.Recipient.Model.postRequest(modelId, APIModel.PutItems(items))
      models.modify { models =>
        val newModel = models.get(modelId).map(_.copy(items = items))
        newModel.fold(models)(models.replace)
      }

    case AddMockModelIds(newIds) =>
      models.modify { models =>
        val newModels = newIds.filterNot(models.contains).map(ModelMock(_))
        models.addAll(newModels)
      }

    case unknownAction =>
      println(s"[ModelsCircuit] Got unknown action: $unknownAction")
      s => s
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
}

object MergeUtility {
  def mergeModelIterables(models: SimpleSet[ID, ModelMock], newModels: Iterable[ModelMock]): SimpleSet[ID, ModelMock] = {
    newModels.foldLeft(models) { case (acc, next) =>
      if (acc.contains(next.id)) acc.modify(mergeModelMocks(_, next))(next)
      else acc + next
    }
  }

  def mergeModelMocks(a: ModelMock, b: ModelMock): ModelMock = {
    val infoInit: Option[ModelInformation] = None
    val newInfo = List(a.info, b.info).flatten.foldLeft(infoInit)((acc, next) => acc.map(_ merge next))

    ModelMock(a.id, newInfo, b.history, List())
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

