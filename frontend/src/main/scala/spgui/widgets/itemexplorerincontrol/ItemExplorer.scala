package spgui.widgets.itemexplorerincontrol

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scalacss.ScalaCssReact._
import spgui.{SPWidget, SPWidgetBase}
import spgui.components.DragAndDrop.{ DataOnDrag, OnDataDrop }
import spgui.communication.BackendCommunication
import spgui.widgets.itemeditor.{API_ItemServiceDummy => api}
import spgui.circuit.{ SPGUICircuit, UpdateGlobalState, GlobalState }
import sp.domain._

import scalajs.js
import js.Dynamic.{literal => l}
import js.JSON
import js.annotation.JSExportTopLevel
import java.util.UUID


object ItemExplorer {

  // TODO temporary way of setting currentModel, currentModel-field will be moved to global state attributes
  /* sp-gui's itemtree already exports this, hence commented out
  @JSExportTopLevel("setCurrentModel")
  def setCurrentModel(modelIDString: String) = {
    val id = UUID.fromString(modelIDString)
    val action = UpdateGlobalState(GlobalState(currentModel = Some(id)))
    SPGUICircuit.dispatch(action)
  }
  */

  import sp.models.{APIModel => mapi}

  def extractMResponse(m: SPMessage) = for {
    h <- m.getHeaderAs[SPHeader]
    b <- m.getBodyAs[mapi.Response]
  } yield (h, b)

  def makeMess(h: SPHeader, b: mapi.Request) = SPMessage.make[SPHeader, mapi.Request](h, b)

  case class ItemExplorerState(structs: List[Struct], modelIDFieldString: String = "modelID")

  class ItemExplorerBackend($: BackendScope[SPWidgetBase, ItemExplorerState]) {

    def prettyPrintStruct(struct: Struct): Unit = {
      println("***********")
      println("Struct " + struct.name)
      struct.items.foreach(sn => println("    Structnode " + sn.nodeID + " has parent " + sn.parent.getOrElse("None")))
    }

    def handleMess(mess: SPMessage): Unit = {
      println("handlemess: " + mess)
      extractMResponse(mess).map{ case (h, b) =>
        val res = b match {
          case tm@mapi.SPItems(items) => {
            val structs = items.map(_.asInstanceOf[Struct])
            println("******************")
            println("items received: " + structs)
            structs.foreach(prettyPrintStruct)
            $.modState(s => s.copy(structs = structs))
          }
          case x => Callback.empty
        }
        res.runNow()
      }
    }

    def sendToModel(model: ID, mess: mapi.Request): Callback = {
      val h = SPHeader(from = "ItemExplorer", to = model.toString,
        reply = SPValue("ItemExplorer"))
      val json = makeMess(h, mess)
      BackendCommunication.publish(json, mapi.topicRequest)
      Callback.empty
    }

    val topic = mapi.topicResponse
    val wsObs = BackendCommunication.getWebSocketStatusObserver(  mess => {
      if (mess) $.props.map(p => p.frontEndState.currentModel.foreach(m => sendToModel(m, mapi.GetStructures))).runNow()
    }, topic)
    val topicHandler = BackendCommunication.getMessageObserver(handleMess, topic)

    def render(p: SPWidgetBase, s: ItemExplorerState) =
      <.div(
        if (p.frontEndState.currentModel.isDefined) renderStructs(s.structs) else renderIfNoModel,
        OnDataDrop(str => Callback.log("dropped " + str + " on item explorer tree"))
      )

    def renderIfNoModel =
      <.div(
        "No model selected. Create a model with som items in ModelsWidget and call setCurrentModel(idString) in console to select one, then open this widget again"
      )

    def renderStructs(structs: List[Struct]) =  <.div(<.ul(structs.toTagMod(s => <.li(renderStruct(s)))))

    def renderStruct(struct: Struct) = {
      val nodeMap = struct.nodeMap
      <.div(
        struct.name,
        "---",
        <.ul(struct.items.filter(_.parent.isEmpty).toTagMod(structNode => <.li(renderStructNode(structNode, struct))))
      )
    }

    def renderStructNode(structNode: StructNode, struct: Struct): TagMod =
      <.div(
        structNode.nodeID.toString,
        <.ul(getChildren(structNode, struct).toTagMod(sn => <.li(renderStructNode(sn, struct))))
      )

    // TODO stole this from StructLogics in spdomain, use that later
    def getChildren(node: StructNode, struct: Struct): List[StructNode] = {
      struct.items.filter(_.parent.contains(node.nodeID))
    }

    def renderItems(items: List[IDAble]) =
      <.div(
        <.ul(
          items.toTagMod(idAble => <.li(idAble.name, DataOnDrag(idAble.id.toString, Callback.log("picked sumthing up"))))
          //items.toTagMod(idAble => <.li(idAble.name))
        )
      )
  }

  val itemExplorerComponent = ScalaComponent.builder[SPWidgetBase]("ItemExplorer")
    .initialState(ItemExplorerState(Nil))
    .renderBackend[ItemExplorerBackend]
    .build

  def apply() = SPWidget { spwb =>
    //println(spwb.frontEndState)
    itemExplorerComponent(spwb)
  }
}
