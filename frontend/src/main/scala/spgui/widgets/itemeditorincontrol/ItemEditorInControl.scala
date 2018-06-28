package spgui.widgets.itemeditorincontrol

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import diode.react.ModelProxy

import scalajs.js
import js.JSON
import spgui.SPWidget
import spgui.components.DragAndDrop.OnDataDrop
import spgui.components.SPWidgetElements
import spgui.circuits.availablemodelscircuit.{AvailableModels, AvailableModelsCircuit}
import spgui.communication.APIComm
import spgui.communication.APIComm._
import sp.domain._
import sp.domain.Logic._
import play.api.libs.json._

import scala.util.{Failure, Success, Try}

object ItemEditorInControl {

  case class Props(proxy: ModelProxy[AvailableModels])
  case class State(models: List[(ID,IDAble)] = List(), mode: String = "code")

  class Backend($: BackendScope[Props, State]) {
    import scala.concurrent.ExecutionContext.Implicits.global
    import sp.models.{APIModel => apimodel}
    val modelcomm = new APIComm[apimodel.Request, apimodel.Response](apimodel.topicRequest,
      apimodel.topicResponse, "ItemEditor", apimodel.service, None, Some(onModelUpdate))

    var jsonEditor: JSONEditor = _ // initialized for real upon mounting, or receiving Item(item)

    // TODO: handle deletion, diffing if mismatch from backend version, etc
    // for now just overwrites items that are changed from the backend.
    def onModelUpdate(h: SPHeader, b: apimodel.Response): Unit = {
      b match {
        case apimodel.ModelUpdate(mid, version, noOfItems, updatedItems, deletedItems, info) =>
          val updateState = $.modState(s => {
            val newCurrentItems = s.models.map {
              case (m, i) if m == mid => (m, updatedItems.find(_.id == i.id).getOrElse(i))
              case x => x
            }
            s.copy(models = newCurrentItems)
          })
          val updateJsonEditor = $.state >>= { s =>
            Callback(jsonEditor.set(JSON.parse(SPValue(s.models.map(_._2)).toJson)))
          }
          (updateState >> updateJsonEditor).runNow()
        case _ => Unit
      }
    }

    def saveItems(state: State): Option[Unit] = {
      for {
        json <- Try { jsonEditor.get() }.toOption
        idAbles <- fromJsonAs[List[IDAble]](JSON.stringify(json)).toOption
      } yield {
        val idsToSave = idAbles.flatMap { item =>
          state.models.find { case (id, oldItem) => id == item.id && item != oldItem }
        }

        idsToSave.groupBy(_._1).foreach { case (model, items) =>
          modelcomm.request(SPHeader(to = model.toString, from = "ItemEditor"),
            apimodel.PutItems(items.map(_._2))).doit.onComplete {
            case Success(_) => println("itemeditor: yay, saved item(s)!")
            case Failure(err) => println("itemeditor: could not save item(s): " + err.toString)
          }
        }
      }
    }

    def requestItem(id: ID): Unit = {
      modelcomm.request(apimodel.GetItem(id)).takeFirstResponse.foreach {
        case (header,apimodel.SPItem(item)) =>
          val updateState = $.modState(s => {
            if(s.models.exists(p=>p._2.id == item.id)) s
            else s.copy(models = s.models :+ (ID.makeID(header.from).get, item))
          })
          val updateJsonEditor = $.state >>= (s => Callback(jsonEditor.set(JSON.parse(SPValue(s.models.map(_._2)).toJson))))
          (updateState >> updateJsonEditor).runNow
        case _ =>
      }
    }

    def toggleMode(oldMode: String): CallbackTo[Unit] = {
      val newMode = if(oldMode == "code") "tree" else "code"
      val updateState = $.modState(s => s.copy(mode = newMode))
      val updateJsonEditor = Callback(jsonEditor.setMode(newMode))
      updateState >> updateJsonEditor
      $.modState(s => {
        val m = Try {
          jsonEditor.setMode(newMode)
          jsonEditor.getMode() // editor only changes if json is valid
        }
        s.copy(mode = m.getOrElse(oldMode))
      })
    }

    val newItems = List("Struct", "Operation", "Thing", "SOPSpec", "SPSpec", "SPResult", "SPState")
    def addNewItem(model: ID, t: String): Callback = {
      val ideable = t match {
        case "Struct" => Struct("")
        case "Operation" => Operation("")
        case "Thing" => Thing("")
        case "SOPSpec" => SOPSpec("", sop = List())
        case "SPSpec" => SPSpec("")
        case "SPResult" => SPResult("")
        case "SPState" => SPState("", state = Map())
        case _ => throw new RuntimeException("should never happen")
      }
      $.modState{ s =>
        val newCurrent = (model, ideable) :: s.models
        jsonEditor.set(JSON.parse(SPValue(newCurrent.map(_._2)).toJson))
        s.copy(models = newCurrent)
      }
    }

    def getOneModel(props: Props, state: State): Option[ID] = {
      // if we only have items loaded from one model, use that model id
      // if there only exist one available model, use that id
      // otherwise, user needs to input
      state.models match {
        case (headId, _) :: t if t.forall { case (modelId, _) => modelId == headId } => Some(headId)
        case _ =>
          val proxyModels = props.proxy().models
          proxyModels.headOption.flatMap { case (id, _ ) => if (proxyModels.size == 1) Some(id) else None }
      }
    }

    def onDrop(s: String): Unit = {
      // drop could be an id...
      val id1 = ID.makeID(s)
      // ... or from the item explorer
      val id2 = (Json.parse(s) \ "idable").asOpt[String].flatMap(ID.makeID)
      (id1 ++ id2).foreach(requestItem)
    }

    def render(props: Props, state: State) =
      <.div(^.height := "100%", // TODO: this is ugly
        OnDataDrop(s => Callback(onDrop(s))),

        <.button(
          ^.className := "btn",
          ^.title := "Save items",
          ^.onClick --> Callback(saveItems(state)),
          <.i(^.className := "fa fa-save")
        ), " ",
        <.button(
          ^.className := "btn",
          ^.title := "Toggle viewing mode",
          ^.onClick --> toggleMode(state.mode),
          if(state.mode == "code") <.i(^.className := "fa fa-tree") else <.i(^.className := "fa fa-edit")
        ),
        SPWidgetElements.dropdown(
          <.i(^.className := "fa fa-plus"),
          newItems.map{m =>
            getOneModel(props, state) match {
              case Some(id) => <.div(m, ^.onClick --> addNewItem(id, m))
              case None =>
                <.div(s"Create $m in ", props.proxy().models.toSeq.map {
                  case (id, name) => <.a(s"$name (${id.toString.take(5)}...)", ^.onClick --> addNewItem(id, m))
                }.mkTagMod(", "))
            }
          })
      )

    def onJSONEditorChange(): CallbackTo[Unit] = {
      // TODO: use this for something...
      $.state >>= { s=>
        val changed = for {
          json <- Try { jsonEditor.get() }.toOption
          idAbles <- fromJsonAs[List[IDAble]](JSON.stringify(json)).toOption
        } yield {
          for {
            newItem <- idAbles
            (m, item) <- s.models if item.id == newItem.id && item != newItem
          } yield {
            (m,newItem)
          }
        }
        changed.foreach { list =>
          println("Item editor change! Unsaved changes: " + list.mkString(","))
        }
        Callback.empty
      }
    }

    def onMount(): CallbackTo[Unit] = {
      $.getDOMNode >>= { domNode =>
        Callback {
          jsonEditor = JSONEditor(domNode, ItemEditorOptions(onJSONEditorChange()))
          jsonEditor.set(JSON.parse("[]"))
        }
      }
    }

    def onUpdate(): CallbackTo[Unit] = {
      // remove ugly menu bar
      $.getDOMNode >>= { domNode =>
        Callback {
          val menuBar = domNode.getElementsByClassName("jsoneditor-menu")
          while(!js.isUndefined(menuBar(0))) menuBar(0).parentNode.removeChild(menuBar(0))
        }
      }
    }
  }

  private val avmcConnection = AvailableModelsCircuit.connect(x => x)

  private val component = ScalaComponent.builder[Props]("ItemEditor")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount())
    .componentDidUpdate(_.backend.onUpdate())
    .build

  def apply() = SPWidget(_ => avmcConnection(proxy => component(Props(proxy))))
}
