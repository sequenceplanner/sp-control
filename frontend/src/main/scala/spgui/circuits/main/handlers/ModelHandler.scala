package spgui.circuits.main.handlers

import diode.{Action, ModelRW}
import monocle.macros.Lenses
import sp.domain
import sp.domain.{ID, IDAble, SPAttributes}
import sp.models.APIModel.ModelInformation
import sp.models.{APIModel, APIModelMaker}
import spgui.SimpleSet
import spgui.communication.ModelCommunication

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
                              items: SimpleSet[ID, IDAble] = SimpleSet[ID, IDAble](_.id)
                            )

@Lenses case class ModelHandlerState(models: SimpleSet[ID, ModelMock], activeModelId: Option[ID], previousActiveModelId: Option[ID]) {
  def activeModel: Option[ModelMock] = activeModelId.map(models.apply)
}

// TODO Someone with domain knowledge needs to take a look at how updates happen.
// TODO It is probably incorrect in several places. For example, state might be
// TODO when it should actually be merged, etc.
class ModelHandler[M](modelRW: ModelRW[M, ModelHandlerState]) extends StateHandler[M, ModelHandlerState, ModelAction](modelRW) {
  import ModelHandlerState.models

  def onAction: PartialFunction[ModelAction, Reaction] = {
    case SaveModel(model) => models.modify(_ + model)

    case RemoveModel(modelId) =>
      react {
        removeModel(modelId)
      } globally {
      ModelCommunication.postRequest(APIModelMaker.DeleteModel(modelId))
    }

    case UpdateModel(model) =>
      // TODO Reflect change in backend
      models.modify(_.replace(model))

    case SetActiveModel(modelId) =>
      updateActiveId(modelId)

    case SetItems(modelId, items) =>
      react {
        models.modify { models =>
          val newModel = models.get(modelId).map(_.copy(items = SimpleSet[ID, IDAble](_.id, items:_*)))
          newModel.fold(models)(models.replace)
        }
      } globally {
        ModelCommunication.postRequest(modelId, APIModel.PutItems(items))
      }

    case AddMockModelIds(newIds) =>
      models.modify { models =>
        val newModels = newIds.filterNot(models.contains).map(ModelMock(_))
        models.addAll(newModels)
      }

    case unknownAction =>
      println(s"[ModelsCircuit] Got unknown action: $unknownAction")
      Reaction.None
  }

  private def updateActiveId(modelId: ID): StateFn = state => {
    if (state.activeModelId.contains(modelId))
      state
    else
      state.copy(previousActiveModelId = state.activeModelId, activeModelId = Some(modelId))
  }

  private def removeModel(modelId: ID): StateFn = models.modify(_.removeByKey(modelId)) andThen (s => {
    if (s.activeModelId.contains(modelId)) {
      s.copy(previousActiveModelId = s.activeModelId, activeModelId = None)
    }
    else s
  })

  override def acceptAction: Action => Boolean = {
    case _: ModelAction => true
    case _ => false
  }
}

object ModelHandler {
  val initialState = ModelHandlerState(SimpleSet(_.id), None, None)
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

    ModelMock(a.id, newInfo, b.history)
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
