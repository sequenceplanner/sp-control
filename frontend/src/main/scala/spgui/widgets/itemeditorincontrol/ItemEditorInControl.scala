package spgui.widgets.itemeditorincontrol

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scalacss.ScalaCssReact._
import scalajs.js
import js.Dynamic.{literal => l}
import js.JSON
import spgui.{SPWidget, SPWidgetBase}
import spgui.components.DragAndDrop.OnDataDrop
import spgui.communication._
import sp.domain._
import sp.domain.Logic._
import java.util.UUID

import scala.util.Try

object ItemEditorInControl {

  case class State(currentModel: Option[ID] = None, currentItem: Option[ID] = None)

  class Backend($: BackendScope[SPWidgetBase, State]) {
    import scala.concurrent.ExecutionContext.Implicits.global
    import sp.models.{APIModel => apimodel}
    // hjälpklass för apikommunikation, kommer nog ändras...
    val modelcomm = new spgui.widgets.labkit.APIComm[apimodel.Request, apimodel.Response](apimodel.topicRequest,
      apimodel.topicResponse, "ItemEditor", apimodel.service, None, Some(onModelUpdate))

    var jsonEditor: JSONEditor = null // initialized for real upon mounting, or receiving Item(item)

    // TODO: handle deletion, diffing if mismatch from backend version, etc
    def onModelUpdate(h: SPHeader,b: apimodel.Response): Unit = {
      val cb = $.state >>= { s =>
        for {
          currentModel <- s.currentModel
          currentItem <- s.currentItem
        } yield {
          b match {
            case apimodel.ModelUpdate(cm, version, noOfItems, updatedItems, deletedItems, info)
                if cm == currentModel =>
              updatedItems.find(_.id == currentItem).foreach { item =>
                jsonEditor.set(JSON.parse(SPValue(item).toJson))
              }
            case x =>
          }
        }
        Callback.empty
      }
      cb.runNow()
    }

    def saveItem(currentModel: ID) = {
      fromJsonAs[IDAble](JSON.stringify(jsonEditor.get())).toOption.foreach { idAble =>
        //val idAble2 = Operation("testSave", id = idAble.id) // för att få en ändring, kunde ej editera själv.
        modelcomm.tell(SPHeader(to = currentModel.toString, from = "ItemEditor"), apimodel.PutItems(List(idAble)))
      }
    }

    def requestItem(id: ID) = {
      modelcomm.ask(apimodel.GetItem(id)).foreach {
        case (header,apimodel.SPItem(item)) =>
          val updateState = $.modState(s => s.copy(currentModel = ID.makeID(header.from), currentItem = Some(id)))
          val makeJsonEditor = $.getDOMNode >>= { domNode => Callback {
            jsonEditor = JSONEditor($.getDOMNode.runNow(), ItemEditorOptions())
          }}
          (updateState >> makeJsonEditor >> $.forceUpdate).runNow
          jsonEditor.set(JSON.parse(SPValue(item).toJson))
        case x =>
      }
    }

    def render(spwb: SPWidgetBase, state: State) =
      <.div(
        <.button("Save", ^.onClick --> Callback(state.currentModel.foreach(saveItem))),
        <.div(
          "drop an item from item explorer tree to edit it",
          OnDataDrop(idAsStr => Callback(ID.makeID(idAsStr).foreach(requestItem)))
        ).when(jsonEditor == null)
      ) // editor added after mount, or on item dropped
  }

  private val component = ScalaComponent.builder[SPWidgetBase]("ItemEditor")
    .initialState(State())
    .renderBackend[Backend]
    /* // this will be used if itemeditor will know what item to edit before its opened
    .componentDidMount(dcb =>
      Callback(dcb.backend.jsonEditor = JSONEditor(dcb.getDOMNode, ItemEditorOptions()))
    )
    */
    .build

  def apply() = SPWidget(spwb => component(spwb))
}
