package spgui.widgets.itemexplorerincontrol

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import diode.react.ModelProxy

import sp.domain._
import spgui.{ SPWidget, SPWidgetBase }
import spgui.components.DragAndDrop.{ DataOnDrag, OnDataDrop }
import spgui.components.{ Icon, SPWidgetElements }
import spgui.communication.BackendCommunication
import spgui.availablemodelscircuit.{ AvailableModelsCircuit, AvailableModels }

import StuffToMoveToStructLogic._


object ItemExplorer {

 import sp.models.{APIModel => mapi}

  def extractMResponse(m: SPMessage) = for {
    h <- m.getHeaderAs[SPHeader]
    b <- m.getBodyAs[mapi.Response]
  } yield (h, b)

  def makeMess(h: SPHeader, b: mapi.Request) = SPMessage.make[SPHeader, mapi.Request](h, b)

  case class ItemExplorerState(
                                currentModel: Option[ID] = None,
                                newItems: List[IDAble] = Nil,
                                structs: List[Struct] = Nil,
                                expandedIDs: Set[ID] = Set(),
                                hiddenIDs: Set[ID] = Set(),
                                retrievedItems: Map[ID, IDAble] = Map(),
                                modelIDFieldString: String = "modelID"
                              )

  class ItemExplorerBackend($: BackendScope[SPWidgetBase, ItemExplorerState]) {

    val topicHandler = BackendCommunication.getMessageObserver(handleMess, mapi.topicResponse)

    def handleMess(mess: SPMessage): Unit = {
      extractMResponse(mess).map { case (h, b) =>
        val res = b match {
          case tm@mapi.SPItems(items) => {
            val structs = items.filter(_.isInstanceOf[Struct]).asInstanceOf[List[Struct]]
            val notStructs = items.filterNot(_.isInstanceOf[Struct]).map(s => s.id -> s).toMap
            if (structs.nonEmpty) $.modState(_.copy(structs = structs))
            else if (notStructs.nonEmpty) $.modState(s => s.copy(retrievedItems = s.retrievedItems ++ notStructs))
            else Callback.empty
          }
          case x => Callback.empty
        }
        res.runNow()
      }
    }

    def sendToModel(mess: mapi.Request): Callback = {
      val currentModel = $.state.map(_.currentModel)
      currentModel.flatMap(op => op.map(id => sendToModel(id, mess)).getOrElse(Callback.empty))
    }

    def sendToModel(id: ID, mess: mapi.Request): Callback = Callback {
      val h = SPHeader(from = "ItemExplorer", to = id.toString, reply = SPValue("ItemExplorer"))
      val json = makeMess(h, mess)
      BackendCommunication.publish(json, mapi.topicRequest)
    }

    /*
    def createItem(kind: String, struct: Struct) = {
      val item = ItemKinds.create(kind)
      val newStruct = addItem(item.id, struct)
      val modifyStateStruct = $.modState(s => s.copy(structs = newStruct :: s.structs.filterNot(_ == struct)))
      val modifyStateItem = $.modState(s => s.copy(retrievedItems = s.retrievedItems + (item.id -> item)))
      val notifyBackend = sendToModel(mapi.PutItems(List(item, newStruct)))
      modifyStateStruct >> modifyStateItem >> notifyBackend
    }
    */
    def createItem(kind: String) = {
      val item = ItemKinds.create(kind)
      $.modState(s => s.copy(newItems = item :: s.newItems))
    }

    def setCurrentModel(id: ID) = { // TODO dont do anything if already active
      val modifyState = $.setState(ItemExplorerState(currentModel = Some(id)))
      val requestStructs = sendToModel(id, mapi.GetStructures) // modifyState doesnt finish before this is called hence the id in
      modifyState >> requestStructs
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
      $.modState(s => s.copy(hiddenIDs = hiddenIDs))
    }

    def toggleID(id: ID) = $.modState(s => s.copy(expandedIDs = s.expandedIDs + id -- s.expandedIDs.intersect(Set(id))))

    def toggleStruct(id: ID) = {
      val childIDs = $.state.map(_.structs.find(_.id == id).get.items.map(_.item).toList)
      val askForStructNodes = childIDs.flatMap(ids => sendToModel(mapi.GetItems(ids)))
      toggleID(id) >> askForStructNodes
    }

    def toggleStructNode(id: ID, struct: Struct) = {
      val theStructNode = struct.items.find(_.nodeID == id).get
      val childIDs = getChildren(theStructNode, struct).map(_.item)
      val askForItems = sendToModel(mapi.GetItems(childIDs))
      toggleID(id) >> askForItems
    }

    def handleDrop(draggedItemNodeID: ID, receivingItemNodeID: ID, struct: Struct) = {
      $.state.map(_.newItems.find(_.id == draggedItemNodeID)).flatMap { newItemOp =>
        newItemOp.map( newItem => moveNewItem(newItem, receivingItemNodeID)).getOrElse {
          val newStruct = moveNode(draggedItemNodeID, receivingItemNodeID, struct)
          val modifyState = $.modState(s => s.copy(structs = newStruct :: s.structs.filterNot(_ == struct)))
          val notifyBackend = sendToModel(mapi.PutItems(List(newStruct)))
          modifyState >> notifyBackend
        }
      }
    }

    def moveNewItem(newItem: IDAble, receivingNodeID: ID) = {
      val struct = $.state.map(_.structs.find(struct => containsNode(receivingNodeID, struct)).get)
      val newStruct = struct.map(s => addNode(StructNode(newItem.id, Some(receivingNodeID)), s))
      val itemToBackend = sendToModel(mapi.PutItems(List(newItem)))
      val moveItemInState = $.modState { s =>
        s.copy(
          newItems = s.newItems.filterNot(_.id == newItem.id),
          retrievedItems = s.retrievedItems + (newItem.id -> newItem)
        )
      }
      newStruct.flatMap(replaceStruct) >> moveItemInState
    }

    // replaces the Struct with same ID as newStruct with newStruct
    def replaceStruct(newStruct: Struct) = {
      val modifyState = $.modState(s => s.copy(structs = newStruct :: s.structs.filterNot(_.id == newStruct.id)))
      val notifyBackend = sendToModel(mapi.PutItems(List(newStruct)))
      modifyState >> notifyBackend
    }

    def render(p: SPWidgetBase, s: ItemExplorerState) =
      <.div(
        ^.className := Style.outerDiv.htmlClass,
        renderOptionPane(s),
        renderNewItems(s),
        renderStructs(s)
      )

    val avmcConnection = AvailableModelsCircuit.connect(x => x)

    def renderOptionPane(state: ItemExplorerState) =
      <.div(
        ^.className := Style.optionPane.htmlClass,
        SPWidgetElements.dropdown(
          Icon.plus,
          ItemKinds.list.map(kind => <.div(kind, ^.onClick --> createItem(kind)))
        ),
        avmcConnection(proxy => ModelChoiceDropdown(proxy, id => setCurrentModel(id))),
        SPWidgetElements.TextBox("Filter...", str => filterItems(str, state.structs(0)))
      )

    def renderNewItems(s: ItemExplorerState) =
      <.div(
        ^.className := Style.newItems.htmlClass,
        <.ul(
          <.li("New Items: "),
          s.newItems.toTagMod(idAble => <.li(DataOnDrag(idAble.id.toString), ItemKinds.icon(idAble), idAble.name))
        )
      ).when(!s.newItems.isEmpty)

    def renderStructs(s: ItemExplorerState) =
      <.div(
        ^.className := Style.structsView.htmlClass,
        <.ul(
          ^.className := Style.ul.htmlClass,
          s.structs.toTagMod(struct => <.li(renderStruct(struct, s)))
        )
      )

    def renderStruct(struct: Struct, state: ItemExplorerState) = {
      val rootItemsToRender = struct.items.filter(sn => sn.parent.isEmpty && !state.hiddenIDs.contains(sn.nodeID))
      <.div(
        <.div(
          Icon.folder,
          struct.name,
          ^.onClick --> toggleStruct(struct.id),
          OnDataDrop(draggedStr => handleDrop(ID.makeID(draggedStr).get, struct.id, struct))
        ),
        <.ul(
          ^.className := Style.ul.htmlClass,
          rootItemsToRender.toTagMod(sn => <.li(renderStructNode(sn, struct, state)))
        ).when(state.expandedIDs.contains(struct.id))
      )
    }

    def renderStructNode(structNode: StructNode, struct: Struct, state: ItemExplorerState): TagMod = {
      val renderedItemOp = state.retrievedItems.get(structNode.item).map(renderItem(_, structNode, struct, state))
      val childrenToRender = getChildren(structNode, struct).filterNot(sn => state.hiddenIDs.contains(sn.nodeID))
      <.div(
        <.div(
          renderedItemOp.getOrElse(structNode.item.toString),
          DataOnDrag(structNode.nodeID.toString),
          OnDataDrop(draggedStr => handleDrop(ID.makeID(draggedStr).get, structNode.nodeID, struct))
        ),
        <.ul(
          ^.className := Style.ul.htmlClass,
          childrenToRender.toTagMod(sn => <.li(renderStructNode(sn, struct, state)))
        ).when(state.expandedIDs.contains(structNode.nodeID))
      )
    }

    def renderItem(item: IDAble, structNode: StructNode, struct: Struct, state: ItemExplorerState) =
      <.div(
        <.span(
          if (state.expandedIDs.contains(structNode.nodeID)) Icon.toggleRight else Icon.toggleDown,
          ^.onClick --> toggleStructNode(structNode.nodeID, struct)
        ),
        ItemKinds.icon(item),
        item.name
      )
  }

  val itemExplorerComponent = ScalaComponent.builder[SPWidgetBase]("ItemExplorer")
    .initialState(ItemExplorerState())
    .renderBackend[ItemExplorerBackend]
    .build

  def apply() = SPWidget(spwb => itemExplorerComponent(spwb))
}

object ModelChoiceDropdown {
  case class Props(proxy: ModelProxy[AvailableModels], cb: ID => Callback)

  val component = ScalaComponent.builder[Props]("ModelChoiceDropdown")
    .render_P { p =>
      val contents = p.proxy().models.toList.map(kv => <.div(kv._1.toString, ^.onClick --> p.cb(kv._1)))
      SPWidgetElements.dropdown("Choose Model", contents)
    }
    .build

  def apply(proxy: ModelProxy[AvailableModels], cb: ID => Callback) = component(Props(proxy, cb))
}

object ItemKinds {
  val list = List("Thing", "Operation", "SOPSpec")

  def icon(item: IDAble): TagMod = item match {
    case _: Thing => Icon.puzzlePiece
    case _: Operation => Icon.arrowCircleRight
    case _: SOPSpec => Icon.sitemap
    case _ => "UNKNOWN KIND OF ITEM"
  }

  def create(kind: String) = {
    val name = "New " + kind
    kind match {
      case "Thing" => Thing(name)
      case "Operation" => Operation(name)
      case "SOPSpec" => SOPSpec(name, sop = List())
      case _ => throw new RuntimeException("ItemKinds case error. This should never happen.")
    }
  }
}
