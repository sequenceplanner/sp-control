package spgui.widgets.itemexplorerincontrol

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import diode.react.ModelProxy

import sp.domain._
import sp.domain.logic.StructLogic._
import spgui.{ SPWidget, SPWidgetBase }
import spgui.components.DragAndDrop.{ DataOnDrag, OnDataDrop }
import spgui.components.{ Icon, SPWidgetElements }
import spgui.communication.APIComm
import spgui.communication.APIComm.StreamHelper
import spgui.availablemodelscircuit.{ AvailableModelsCircuit, AvailableModels }

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


object ItemExplorer {

  import sp.models.{ APIModel => mapi }

  def makeMess(h: SPHeader, b: mapi.Request) = SPMessage.make[SPHeader, mapi.Request](h, b)

  class ModelComm(val modelID: ID) extends
    APIComm[mapi.Request, mapi.Response](
      requestTopic = mapi.topicRequest,
      responseTopic = mapi.topicResponse,
      from = "ItemExplorer",
      to = modelID.toString,
      onChannelUp = None,
      onMessage = None
    )

  case class ItemExplorerState(
                                currentMComm: Option[ModelComm] = None,
                                newItems: List[IDAble] = Nil,
                                structs: List[Struct] = Nil,
                                expandedIDs: Set[ID] = Set(),
                                hiddenIDs: Map[ID, Set[ID]] = Map(), // structID -> structNodeIDs
                                retrievedItems: Map[ID, IDAble] = Map(),
                                modelIDFieldString: String = "modelID",
                                expanded: Boolean = false
                              )

  class ItemExplorerBackend($: BackendScope[SPWidgetBase, ItemExplorerState]) {

    def sendToModel(mess: mapi.Request): Callback = {
      val currentMComm = $.state.map(_.currentMComm)
      currentMComm.flatMap(op => op.map(comm => Callback(comm.request(mess))).getOrElse(Callback.empty))
    }

    def request(req: mapi.Request): Future[mapi.Response] = {
      val mcomm = $.state.map(_.currentMComm.get).toFuture
      mcomm.flatMap(_.request(req).takeFirstResponse.map(_._2))
    }

    def itemRequest(ids: Set[ID]): Future[Set[IDAble]] = {
      val mcomm = $.state.map(_.currentMComm.get).toFuture
      val futureToRes = mcomm.flatMap(_.request(mapi.GetItems(ids.toList)).takeFirstResponse.map(_._2))
      futureToRes.map {
        case mapi.SPItems(items) => items.toSet
        case _ => Set[IDAble]()
      }
    }

    def retrieveItems(ids: List[ID]) = Callback.future {
      val future = request(mapi.GetItems(ids))
      future.map {
        case x: mapi.SPItems =>
          $.modState(s => s.copy(retrievedItems = s.retrievedItems ++ x.items.map(it => it.id -> it).toMap))
        case _ =>
          Callback.empty
      }
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
      val mcomm = new ModelComm(id)
      val modifyState = $.setState(ItemExplorerState(currentMComm = Some(mcomm)))
      val requestStructs = Callback.future {
        val f = mcomm.request(mapi.GetStructures).takeFirstResponse
        f.map(_._2).map {
          case b: mapi.SPItems =>
            $.modState { s =>
              s.copy(
                structs = b.items.asInstanceOf[List[Struct]],
                hiddenIDs = s.hiddenIDs ++ b.items.map(_.id -> Set[ID]()).toMap
              )
            }
          case _ => Callback.empty
        }
      }
      modifyState >> requestStructs
    }

    def filterAllStructs(query: String) = {
      $.state.map(_.structs).flatMap(list => Callback.sequence(list.map(struct => filterItems(query, struct))))
    }

    def filterItems(query: String, struct: Struct) = Callback.future {
      val future = request(mapi.StructFilter(struct.id, query))
      future.map {
        case mapi.FilteredStruct(hiddenNodes) =>
          $.modState(s => s.copy(hiddenIDs = s.hiddenIDs + (struct.id -> hiddenNodes)))
        case _ =>
          Callback.empty
      }
    }

    def toggleID(id: ID) = $.modState(s => s.copy(expandedIDs = s.expandedIDs + id -- s.expandedIDs.intersect(Set(id))))

    /*
    def toggleAll = {
      val toggleStructs = $.state.map(_.structs).flatMap(list => Callback.sequence(list.map(s => toggleStruct(s.id))))
      val allNodeIDs = $.state.map(_.structs.flatMap(_.items.map(_.nodeID)))
      val toggleNodes = allNodeIDs.flatMap(ids => $.modState(s => s.copy(expandedIDs = s.expandedIDs ++ ids.toSet)))
      toggleStructs >> toggleNodes
    }
    */
    def toggleAll = $.modState(s => s.copy(expanded = !s.expanded))

    def toggleStruct(id: ID) = {
      val childIDs = $.state.map(_.structs.find(_.id == id).get.items.collect{case x if x.parent.isEmpty => x.item}.toList)
      val fetchItems = childIDs.flatMap(retrieveItems(_))
      toggleID(id) >> fetchItems
    }

    def toggleStructNode(id: ID, struct: Struct) = {
      val childIDs = struct.getChildren(id).map(_.item).toList
      toggleID(id) >> retrieveItems(childIDs)
    }

    def handleDrop(draggedNodeID: ID, receivingNodeID: ID) = {
      // TODO implement moving of node to root level of struct
      $.state.map(_.newItems.find(_.id == draggedNodeID)).flatMap { newItemOp => // if item among new items
        newItemOp.map(moveNewItem(_, receivingNodeID))
          .getOrElse { // else move node within struct or between structs
            val previousStruct = $.state.map(_.structs.find(_.items.exists(_.nodeID == draggedNodeID)).get)
            val targetStruct = $.state.map(_.structs.find(_.items.exists(_.nodeID == receivingNodeID)).get)
            previousStruct.zip(targetStruct).flatMap { case (ps, ts) =>
              if (ps.id == ts.id) replaceStruct(ps.moveNode(draggedNodeID, receivingNodeID))
              else moveItemBetweenStructs(draggedNodeID, receivingNodeID, ps, ts)
            }
          }
      }
    }

    def moveNewItem(newItem: IDAble, receivingNodeID: ID) = {
      val struct = $.state.map(_.structs.find(struct => struct.items.exists(_.nodeID == receivingNodeID)).get)
      val newStruct = struct.map(_.addTo(receivingNodeID, Set(StructNode(newItem.id))))
      val itemToBackend = sendToModel(mapi.PutItems(List(newItem)))
      val moveItemInState = $.modState { s =>
        s.copy(
          newItems = s.newItems.filterNot(_.id == newItem.id),
          retrievedItems = s.retrievedItems + (newItem.id -> newItem)
        )
      }
      newStruct.flatMap(replaceStruct) >> moveItemInState >> itemToBackend
    }

    //def moveItemWithinStruct(draggedNodeID: ID, receivingNodeID: ID, struct: Struct) =
      //replaceStruct(moveNode(draggedNodeID, receivingNodeID, struct))

    // subtle parent-ID stuff going on here.. should perhaps be moved to StructLogic
    def moveItemBetweenStructs(draggedID: ID, receivingID: ID, fromStruct: Struct, targetStruct: Struct) = {
      val draggedNode = fromStruct.items.find(_.nodeID == draggedID)
      val nodesToMove = draggedNode.map(fromStruct.getAllChildren(draggedID) + _.copy(parent = None))
      val newFromStructOp = nodesToMove.map(fromStruct -- _.map(_.nodeID))
      val newTargetStructOp = nodesToMove.map(targetStruct.addTo(receivingID, _))
      val zipOp = newFromStructOp.zip(newTargetStructOp).headOption
      val (newFromStruct, newTargetStruct) = zipOp.getOrElse((fromStruct, targetStruct))
      replaceStruct(newFromStruct) >> replaceStruct(newTargetStruct)
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
        SPWidgetElements.button(Icon.expand, toggleAll),
        SPWidgetElements.dropdown(
          Icon.plus,
          ItemKinds.list.map(kind => <.div(kind, ^.onClick --> createItem(kind)))
        ),
        avmcConnection(proxy => ModelChoiceDropdown(proxy, id => setCurrentModel(id))),
        SPWidgetElements.TextBox("Filter...", str => filterAllStructs(str))
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
          s.structs.toTagMod { struct =>
            <.li(
              StructView(
                struct,
                retrieveItems = Some(itemRequest),
                handleDrop = Some(handleDrop),
                filteredNodes = s.hiddenIDs(struct.id),
                expanded = s.expanded
              )
            )
          }
        )
      )
  }

  val itemExplorerComponent = ScalaComponent.builder[SPWidgetBase]("ItemExplorer")
    .initialState(ItemExplorerState())
    .renderBackend[ItemExplorerBackend]
    .build

  def apply() = SPWidget(spwb => itemExplorerComponent(spwb))
}

object StructView {
  case class Props(
                    struct: Struct,
                    items: Map[ID, IDAble],
                    retrieveItems: Option[Set[ID] => Future[Set[IDAble]]],
                    handleDrop: Option[(ID, ID) => Callback],
                    filteredNodes: Set[ID],
                    expanded: Boolean
                  )
  case class State(
                    items: Map[ID, IDAble],
                    expandedNodes: Set[ID] = Set()
                  )

  class Backend($: BackendScope[Props, State]) {

    def toggle(id: ID, childrenIDs: Set[ID]) = {
      val modNodes = $.modState(s => s.copy(expandedNodes = s.expandedNodes + id -- s.expandedNodes.intersect(Set(id))))
      retrieveItems(childrenIDs) >> modNodes
    }

    def retrieveItems(ids: Set[ID]) = {
      def addToState(items: Set[IDAble]) =
        $.modState(s => s.copy(items = s.items ++ items.map(item => item.id -> item)))
      val future = $.props.map(_.retrieveItems.map(_(ids).map(addToState)).getOrElse(Future(Callback.empty)))
      future.flatMap(Callback.future(_))
    }

    def render(p: Props, s: State) = {
      val dragHandling = p.handleDrop.map { handleDropFunction =>
        OnDataDrop(draggedStr => handleDropFunction(ID.makeID(draggedStr).get, p.struct.id))
      }.getOrElse(EmptyVdom)
      val rootItemsToRender = p.struct.items.filter(sn => sn.parent.isEmpty && !p.filteredNodes.contains(sn.nodeID))
      lazy val directChildren = p.struct.items.filter(_.parent == None).map(_.item)

      <.div(
        <.div(^.onClick --> toggle(p.struct.id, directChildren), dragHandling, Icon.folder, p.struct.name),
        <.ul(
          ^.className := Style.ul.htmlClass,
          rootItemsToRender.toTagMod(node => <.li(renderNode(node, p, s)))
        ).when(s.expandedNodes.contains(p.struct.id))
      )
    }

    def renderNode(node: StructNode, p: Props, s: State): TagMod = {
      val dragHandling = p.handleDrop.map { handleDropFunction =>
        List(
          DataOnDrag(node.nodeID.toString),
          OnDataDrop(draggedStr => handleDropFunction(ID.makeID(draggedStr).get, node.nodeID))
        ).toTagMod
      }.getOrElse(EmptyVdom)
      val childrenToRender = p.struct.getChildren(node.nodeID).filterNot(sn => p.filteredNodes.contains(sn.nodeID))

      <.div(
        <.div(renderNodeItem(node, p, s), dragHandling),
        <.ul(
          ^.className := Style.ul.htmlClass,
          childrenToRender.toTagMod(sn => <.li(renderNode(sn, p, s)))
        ).when(s.expandedNodes.contains(node.nodeID))
      )
    }

    def renderNodeItem(node: StructNode, p: Props, s: State) = {
      val arrowIcon = if (s.expandedNodes.contains(node.nodeID)) Icon.toggleRight else Icon.toggleDown
      val itemOp = s.items.get(node.item)
      val itemIcon = itemOp.map(ItemKinds.icon(_)).getOrElse(Icon.question)
      val shownName = itemOp.map(_.name).getOrElse(node.item.toString)
      lazy val directChildren = p.struct.getChildren(node.nodeID).map(_.item)

      <.div(<.span(arrowIcon, ^.onClick --> toggle(node.nodeID, directChildren)), itemIcon, shownName)
    }
  }

  val component = ScalaComponent.builder[Props]("StructView")
    .initialStateFromProps(p => State(p.items))
    .renderBackend[Backend]
    .componentWillReceiveProps { scope => // TODO retrieve all items
      val nextExpanded = scope.nextProps.expanded
      val expandedChanged = scope.currentProps.expanded != nextExpanded
      if (expandedChanged) {
        if (nextExpanded) {
          val allNodeIDs = scope.currentProps.struct.items.map(_.nodeID)
          val structID = scope.currentProps.struct.id
          scope.modState(_.copy(expandedNodes = allNodeIDs + structID))
        } else {
          scope.modState(_.copy(expandedNodes = Set()))
        }
      }
      else {
        Callback.empty
      }
    }
    .build

  def apply(
             struct: Struct,
             items: Map[ID, IDAble] = Map(),
             retrieveItems: Option[Set[ID] => Future[Set[IDAble]]] = None,
             handleDrop: Option[(ID, ID) => Callback] = None,
             filteredNodes: Set[ID] = Set(),
             expanded: Boolean = false
           ) = component(Props(struct, items, retrieveItems, handleDrop, filteredNodes, expanded))
}

object ModelChoiceDropdown {
  case class Props(proxy: ModelProxy[AvailableModels], cb: ID => Callback)

  val component = ScalaComponent.builder[Props]("ModelChoiceDropdown")
    .render_P { p =>
      val contents = p.proxy().models.toList.map(kv => <.div(kv._2, ^.onClick --> p.cb(kv._1)))
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
