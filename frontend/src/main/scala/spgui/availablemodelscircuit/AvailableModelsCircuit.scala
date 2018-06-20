package spgui.availablemodelscircuit

import diode._
import diode.react._
import sp.domain.{ID, SPHeader}
import sp.models.{APIModel, APIModelMaker}
import spgui.communication.APIComm._
import spgui.communication._

import scala.concurrent.ExecutionContext.Implicits.global

case class AvailableModels(models: Map[ID, String] = Map())

// if widgets are to be allowed to dispatch, remove protected and implement Effects
protected case class AddModels(models: Map[ID, String]) extends Action
protected case class RemoveModels(models: Set[ID]) extends Action

object AvailableModelsCircuit extends Circuit[AvailableModels] with ReactConnector[AvailableModels] {
  val avmh = AvailableModelsHelper // makes sure its code is run

  override def initialModel = AvailableModels()

  val modelsHandler = new ActionHandler(zoomTo(_.models)) {
    override def handle = {
      case AddModels(models) => updated(value ++ models)
      case RemoveModels(models) => updated(value -- models)
    }
  }

  override def actionHandler = composeHandlers(modelsHandler)
}

object AvailableModelsHelper {
  val from = "AvailableModelsCircuit"
  private val modelMakerCommunication = {
    import sp.models.{APIModelMaker => api}
    new APIComm[api.Request, api.Response](
      requestTopic = api.topicRequest,
      responseTopic = api.topicResponse,
      from = from,
      to = api.service,
      onChannelUp = Some(() => onUp()),
      onMessage = Some(onChange)
    )
  }

  private val modelCommunication = {
    import sp.models.{ APIModel => api }

    new APIComm[api.Request, api.Response](
      api.topicRequest, api.topicResponse, from, api.service, None, None
    )
  }

  import spgui.availablemodelscircuit.{AvailableModelsCircuit => avmc}

  def onChange(header: SPHeader, body: APIModelMaker.Response): Unit = {
    body match {
      case APIModelMaker.ModelCreated(name, attr, modelid) =>
        avmc.dispatch(AddModels(Map(modelid -> name)))
      case APIModelMaker.ModelDeleted(modelid) =>
        avmc.dispatch(RemoveModels(Set(modelid)))
      case _ => ()
    }
  }

  def onUp(): Unit = {
    modelMakerCommunication.request(APIModelMaker.GetModels).takeFirstResponse.foreach {
      case (_, APIModelMaker.ModelList(models)) =>
        // add previously unknown models
        val newModels = models.toSet.diff(avmc.zoom(_.models).value.keySet).toSeq
        avmc.dispatch(AddModels(newModels.map(id => id -> "fetching").toMap))
        // ask for model names
        newModels.foreach { id =>
          modelCommunication.request(SPHeader(from = from, to = id.toString), APIModel.GetModelInfo).takeFirstResponse.foreach {
            case (_, APIModel.ModelInformation(name, id, _, _, _)) =>
              avmc.dispatch(AddModels(Map(id -> name)))
            case _ => ()
          }
        }
      case _ => ()
    }
  }
}
