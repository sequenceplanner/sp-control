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

import spgui.communication._
import spgui.communication.APIComm._

import scala.util.{Try,Success, Failure}

object ItemEditorInControl {
  case class State(currentItems: List[(ID,IDAble)] = List(), mode: String = "code")

  class Backend($: BackendScope[SPWidgetBase, State]) {
    import scala.concurrent.ExecutionContext.Implicits.global
    import sp.models.{APIModel => apimodel}
    val modelcomm = new APIComm[apimodel.Request, apimodel.Response](apimodel.topicRequest,
      apimodel.topicResponse, "ItemEditor", apimodel.service, None, Some(onModelUpdate))

    var jsonEditor: JSONEditor = null // initialized for real upon mounting, or receiving Item(item)

    // TODO: handle deletion, diffing if mismatch from backend version, etc
    // for now just overwrites items that are changed from the backend.
    def onModelUpdate(h: SPHeader,b: apimodel.Response): Unit = {
      b match {
        case apimodel.ModelUpdate(mid, version, noOfItems, updatedItems, deletedItems, info) => {
          val updateState = $.modState(s => {
            val newCurrentItems = s.currentItems.map {
              case (m, i) if m == mid => (m, updatedItems.find(_.id == i.id).getOrElse(i))
              case x => x
            }
            s.copy(currentItems = newCurrentItems)
          })
          val updateJsonEditor = $.state >>= { s =>
            Callback(jsonEditor.set(JSON.parse(SPValue(s.currentItems.map(_._2)).toJson)))
          }
          (updateState >> updateJsonEditor).runNow()
        }
        case x =>
      }
    }

    def saveItems(s: State) = {
      for {
        json <- Try { jsonEditor.get() }.toOption
        idAbles <- fromJsonAs[List[IDAble]](JSON.stringify(json)).toOption
      } yield {
        val idsToSave = (for {
          newItem <- idAbles
          (m, item) <- s.currentItems if item.id == newItem.id && item != newItem
        } yield {
          (m,newItem)
        })
        idsToSave.groupBy(_._1).foreach { case (model, items) =>
          modelcomm.request(SPHeader(to = model.toString, from = "ItemEditor"),
            apimodel.PutItems(items.map(_._2))).doit.onComplete {
            case Success(_) => println("yay, saved!")
            case Failure(err) => println("nay: " + err.toString)
          }
        }
      }
    }

    def requestItem(id: ID) = {
      modelcomm.request(apimodel.GetItem(id)).takeFirstResponse.onComplete {
        case Success((header,apimodel.SPItem(item))) =>
          val updateState = $.modState(s => {
            if(s.currentItems.exists(p=>p._2.id == item.id)) s
            else s.copy(currentItems = s.currentItems :+ (ID.makeID(header.from).get, item))
          })
          val updateJsonEditor = $.state >>= (s => Callback(jsonEditor.set(JSON.parse(SPValue(s.currentItems.map(_._2)).toJson))))
          (updateState >> updateJsonEditor).runNow
        case x => println("itemeditor: failed to save: " + x)
      }
    }

    def toggleMode(oldMode: String) = {
      val newMode = if(oldMode == "code") "tree" else "code"
      val updateState = $.modState(s => s.copy(mode = newMode))
      val updateJsonEditor = Callback(jsonEditor.setMode(newMode))
      updateState >> updateJsonEditor
    }

    def render(spwb: SPWidgetBase, state: State) =
      <.div(^.height := "100%", // TODO: this is ugly
        OnDataDrop(idAsStr => Callback(ID.makeID(idAsStr).foreach(requestItem))),

        <.button(
          ^.className := "btn btn-sm",
          ^.title := "Save items",
          ^.onClick --> Callback(saveItems(state)),
          <.i(^.className := "fa fa-save")
        ), " ",
        <.button(
          ^.className := "btn btn-sm",
          ^.title := "Toggle viewing mode",
          ^.onClick --> toggleMode(state.mode),
          if(state.mode == "code") <.i(^.className := "fa fa-tree") else <.i(^.className := "fa fa-edit")
        )
      )

    def onMount() = {
      $.getDOMNode >>= { domNode =>
        Callback {
          jsonEditor = JSONEditor(domNode, ItemEditorOptions())
        }
      }
    }

    def onUpdate() = {
      // remove ugly menu bar
      $.getDOMNode >>= { domNode =>
        Callback {
          val menuBar = domNode.getElementsByClassName("jsoneditor-menu")
          while(!js.isUndefined(menuBar(0))) menuBar(0).parentNode.removeChild(menuBar(0))
        }
      }
    }
  }

  private val component = ScalaComponent.builder[SPWidgetBase]("ItemEditor")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount)
    .componentDidUpdate(_.backend.onUpdate)
    .build

  def apply() = SPWidget(spwb => component(spwb))
}
