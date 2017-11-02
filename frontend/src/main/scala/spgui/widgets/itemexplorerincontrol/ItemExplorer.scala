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

  case class ItemExplorerState(
                                structs: List[Struct],
                                expandedIDs: Set[ID] = Set(),
                                retrievedItems: Map[ID, IDAble] = Map(),
                                modelIDFieldString: String = "modelID"
                              )

  class ItemExplorerBackend($: BackendScope[SPWidgetBase, ItemExplorerState]) {

    def prettyPrintStruct(struct: Struct): Unit = {
      println("***********")
      println("Struct " + struct.name)
      struct.items.foreach(sn => println("    Structnode " + sn.nodeID + " has parent " + sn.parent.getOrElse("None")))
    }

    def handleMess(mess: SPMessage): Unit = {
      println("handlemess: " + mess)
      extractMResponse(mess).map { case (h, b) =>
        val res = b match {
          case tm@mapi.SPItems(items) => {
            val structs = items.filter(_.isInstanceOf[Struct]).asInstanceOf[List[Struct]]
            val notStructs = items.filterNot(_.isInstanceOf[Struct]).map(s => s.id -> s).toMap
            println("******************")
            println("items received: " + structs)
            structs.foreach(prettyPrintStruct)
            $.modState(s => s.copy(structs = structs, retrievedItems = s.retrievedItems ++ notStructs))
            if (structs.nonEmpty) $.modState(_.copy(structs = structs))
            else if (notStructs.nonEmpty) $.modState(s => s.copy(retrievedItems = s.retrievedItems ++ notStructs))
            else Callback.empty
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
    val wsObs = BackendCommunication.getWebSocketStatusObserver(mess => {
      if (mess) $.props.map(p => p.frontEndState.currentModel.foreach(m => sendToModel(m, mapi.GetStructures))).runNow()
    }, topic)
    val topicHandler = BackendCommunication.getMessageObserver(handleMess, topic)


    def toggleStruct(id: ID) = $.modState { state =>
      val theStruct = state.structs.find(_.id == id).get
      val childIDs = theStruct.items.map(_.item)
      val retrieveStructNodes = $.props.map(p => p.frontEndState.currentModel.foreach(m => sendToModel(m, mapi.GetItems(childIDs))))
      retrieveStructNodes.runNow()
      if (state.expandedIDs.contains(id)) state.copy(expandedIDs = state.expandedIDs - id)
      else state.copy(expandedIDs = state.expandedIDs + id)
    }

    def toggleStructNode(id: ID, struct: Struct) = $.modState { state =>
      val theStructNode = struct.items.find(_.nodeID == id).get
      val childIDs = getChildren(theStructNode, struct).map(_.item)
      val retrieveItems = $.props.map(p => p.frontEndState.currentModel.foreach(m => sendToModel(m, mapi.GetItems(childIDs))))
      retrieveItems.runNow()
      if (state.expandedIDs.contains(id)) state.copy(expandedIDs = state.expandedIDs - id)
      else state.copy(expandedIDs = state.expandedIDs + id)
    }

    // TODO stole this from StructLogics in spdomain, use that later
    def getChildren(node: StructNode, struct: Struct): List[StructNode] = {
      struct.items.filter(_.parent.contains(node.nodeID))
    }


    def render(p: SPWidgetBase, s: ItemExplorerState) =
      <.div(
        if (p.frontEndState.currentModel.isDefined) renderStructs(s) else renderIfNoModel,
        OnDataDrop(str => Callback.log("dropped " + str + " on item explorer tree"))
      )

    def renderIfNoModel =
      <.div(
        "No model selected. Create a model with som items in ModelsWidget and call setCurrentModel(idString) in console to select one, then open this widget again"
      )

    def renderStructs(s: ItemExplorerState) =
      <.div(<.ul(s.structs.toTagMod(struct => <.li(renderStruct(struct, s)))))

    def renderStruct(struct: Struct, state: ItemExplorerState) = {
      val nodeMap = struct.nodeMap
      <.div(
        <.div(
          struct.name + " ---",
          ^.onClick --> toggleStruct(struct.id)
        ),
        <.ul(
          struct.items.filter(_.parent.isEmpty).toTagMod(structNode => <.li(renderStructNode(structNode, struct, state)))
        ).when(state.expandedIDs.contains(struct.id))
      )
    }

    def renderStructNode(structNode: StructNode, struct: Struct, state: ItemExplorerState): TagMod = {
      val shownName = state.retrievedItems.get(structNode.item).map(_.name).getOrElse(structNode.item.toString)
      <.div(
        <.div(
          shownName,
          ^.onClick --> toggleStructNode(structNode.nodeID, struct)
        ),
        <.ul(
          getChildren(structNode, struct).toTagMod(sn => <.li(renderStructNode(sn, struct, state)))
        ).when(state.expandedIDs.contains(structNode.nodeID))
      )
    }
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
