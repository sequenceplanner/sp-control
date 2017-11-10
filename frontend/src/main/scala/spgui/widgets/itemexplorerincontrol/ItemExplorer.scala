package spgui.widgets.itemexplorerincontrol

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scalacss.ScalaCssReact._
import spgui.{SPWidget, SPWidgetBase}
import spgui.components.DragAndDrop.{ DataOnDrag, OnDataDrop }
import spgui.components.{ Icon, SPWidgetElements }
import spgui.communication.BackendCommunication
import spgui.widgets.itemeditor.{API_ItemServiceDummy => api}
import spgui.circuit.{ SPGUICircuit, UpdateGlobalState, GlobalState }
import sp.domain._
import StuffToMoveToStructLogic._

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
                                hiddenIDs: Set[ID] = Set(),
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

    def createItem(kind: String, struct: Struct) = {
      val item = kind match {
        case "Thing" => Thing("New Thing")
        case _ => throw new RuntimeException("Itemexplorer case error. Should never happen.")
      }

      val newStruct = addItem(item.id, struct)
      val modifyStateStruct = $.modState(s => s.copy(structs = newStruct :: s.structs.filterNot(_ == struct)))
      val modifyStateItem = $.modState(s => s.copy(retrievedItems = s.retrievedItems + (item.id -> item)))
      val notifyBackend = $.props.map(p => p.frontEndState.currentModel.foreach(m => sendToModel(m, mapi.PutItems(List(item, newStruct)))))
      modifyStateStruct >> modifyStateItem >> notifyBackend
    }

    def filterItems(filterString: String, struct: Struct) = {
      def filteredNodeIDs(filterString: String, struct: Struct) = { // TODO move to backend, where it has access to items and is faster
        val matchingNodes = struct.items.filter(_.nodeID.toString.contains(filterString.toLowerCase)).map(_.nodeID)
        def getParents(id: ID): Set[ID] = {
          val parentOp = struct.nodeMap(id).parent
          if (parentOp.isDefined) getParents(parentOp.get) + parentOp.get
          else Set[ID]()
        }
        val parentsOfMatchingNodes: Set[ID] = matchingNodes.flatMap(getParents)
        struct.items.map(_.nodeID) -- matchingNodes -- parentsOfMatchingNodes
      }
      val hiddenIDs = filteredNodeIDs(filterString, struct)
      val log = Callback.log("in filterItems... string: " + filterString)
      val modifyState = $.modState(s => s.copy(hiddenIDs = hiddenIDs))
      modifyState >> log
    }

    def toggleStruct(id: ID) = $.modState { state =>
      val theStruct = state.structs.find(_.id == id).get
      val childIDs = theStruct.items.map(_.item).toList
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

    def printDrop(draggedItemNodeID: ID, receivingItemNodeID: ID, struct: Struct) = $.state.flatMap { state =>
      val draggedItemID = struct.items.find(_.nodeID == draggedItemNodeID).get.item
      val draggedItemName = state.retrievedItems(draggedItemID).name
      val receivingItemID = struct.items.find(_.nodeID == receivingItemNodeID).map(_.item)
      val receivingItemName = receivingItemID.flatMap(id => state.retrievedItems.get(id).map(_.name))
        .getOrElse(state.structs.find(_.id == receivingItemNodeID).get.name)
      Callback.log("dropped " + draggedItemName + " on " + receivingItemName)
    }

    // TODO no validation of move anywhere yet
    def handleDrop(draggedItemNodeID: ID, receivingItemNodeID: ID, struct: Struct) = {
      val newStruct = moveNode(draggedItemNodeID, receivingItemNodeID, struct)
      val modifyState = $.modState(s => s.copy(structs = newStruct :: s.structs.filterNot(_ == struct)))
      val notifyBackend = $.props.map(p => p.frontEndState.currentModel.foreach(m => sendToModel(m, mapi.PutItems(List(newStruct)))))
      modifyState >> notifyBackend
    }

    def render(p: SPWidgetBase, s: ItemExplorerState) =
      <.div(
        renderOptionPane(s),
        if (p.frontEndState.currentModel.isDefined) renderStructs(s) else renderIfNoModel
      )

    def renderOptionPane(state: ItemExplorerState) =
      <.div(
        Style.optionPane,
        Icon.plus,
        SPWidgetElements.dropdown(
          "Add Item", // TODO need to change SPWidgetElements to be able to use icon here
          List(
            ("Thing", ^.onClick --> createItem("Thing", state.structs(0)))
          ).map(x => <.div(x._1, x._2))
        ),
        SPWidgetElements.TextBox("Filter...", str => filterItems(str, state.structs(0)))
      )


    def renderIfNoModel =
      <.div(
        "No model selected. Create a model with som items in ModelsWidget and call setCurrentModel(idString) in console to select one, then open this widget again"
      )

    def renderStructs(s: ItemExplorerState) =
      <.div(<.ul(s.structs.toTagMod(struct => <.li(renderStruct(struct, s)))))

    def renderStruct(struct: Struct, state: ItemExplorerState) = {
      val nodeMap = struct.nodeMap
      val rootItemsToRender = struct.items.filter(sn => sn.parent.isEmpty && !state.hiddenIDs.contains(sn.nodeID))
      <.div(
        <.div(
          Icon.folder,
          struct.name,
          ^.onClick --> toggleStruct(struct.id),
          OnDataDrop(draggedStr => handleDrop(ID.makeID(draggedStr).get, struct.id, struct))
        ),
        <.ul(
          rootItemsToRender.toTagMod(sn => <.li(renderStructNode(sn, struct, state)))
        ).when(state.expandedIDs.contains(struct.id))
      )
    }

    def renderStructNode(structNode: StructNode, struct: Struct, state: ItemExplorerState): TagMod = {
      val renderedItemOp = state.retrievedItems.get(structNode.item).map(renderItem)
      val childrenToRender = getChildren(structNode, struct).filterNot(sn => state.hiddenIDs.contains(sn.nodeID))
      <.div(
        <.div(
          renderedItemOp.getOrElse(structNode.item.toString),
          ^.onClick --> toggleStructNode(structNode.nodeID, struct),
          DataOnDrag(structNode.nodeID.toString),
          OnDataDrop(draggedStr => handleDrop(ID.makeID(draggedStr).get, structNode.nodeID, struct))
        ),
        <.ul(
          childrenToRender.toTagMod(sn => <.li(renderStructNode(sn, struct, state)))
        ).when(state.expandedIDs.contains(structNode.nodeID))
      )
    }

    def renderItem(item: IDAble): TagMod = {
      val icon: TagMod = item match {
        case _: Operation => Icon.arrowCircleRight
        case _: SOPSpec => Icon.sitemap
        case _ => "design not chosen (TODO) "
      }
      <.div(icon, item.name)
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
