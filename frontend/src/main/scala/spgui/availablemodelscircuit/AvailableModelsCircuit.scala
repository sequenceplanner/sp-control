package spgui.availablemodelscircuit

import diode._
import diode.react._

import sp.domain.{ ID, SPHeader }
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
  import sp.models.{ APIModelMaker => apimm }
  val mmcomm = new APIComm[apimm.Request, apimm.Response](
    apimm.topicRequest, apimm.topicResponse, from, apimm.service, Some(() => onUp()), Some(onChange)
  )

  import sp.models.{ APIModel => apim }
  val mcomm = new APIComm[apim.Request, apim.Response](
    apim.topicRequest, apim.topicResponse, from, apim.service, None, None
  )

  import spgui.availablemodelscircuit.{AvailableModelsCircuit => avmc}

  def onChange(header: SPHeader, body: apimm.Response): Unit = {
    body match {
      case apimm.ModelCreated(name, attr, modelid) =>
        avmc.dispatch(AddModels(Map(modelid -> name)))
      case apimm.ModelDeleted(modelid) =>
        avmc.dispatch(RemoveModels(Set(modelid)))
      case _ => ()
    }
  }

  def onUp(): Unit = {
    mmcomm.request(apimm.GetModels).takeFirstResponse.foreach {
      case (_, apimm.ModelList(models)) =>
        // add previously unknown models
        val newModels = models.toSet.diff(avmc.zoom(_.models).value.keySet).toSeq
        avmc.dispatch(AddModels(newModels.map(id => id -> "fetching").toMap))
        // ask for model names
        newModels.foreach { id =>
          mcomm.request(SPHeader(from = from, to = id.toString), apim.GetModelInfo).takeFirstResponse.foreach {
            case (_, apim.ModelInformation(name, id, _, _, _)) =>
              avmc.dispatch(AddModels(Map(id -> name)))
            case _ => ()
          }
        }
      case _ => ()
    }
  }
}
