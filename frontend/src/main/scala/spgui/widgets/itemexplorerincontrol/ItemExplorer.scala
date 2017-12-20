package spgui.widgets.itemexplorerincontrol

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._
import sp.domain.logic.StructLogic._
import sp.models.{APIModel => mapi}
import spgui.SPWidget
import spgui.components.{Icon, SPWidgetElements}
import spgui.communication.APIComm.StreamHelper

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ItemExplorer {
  case class State(
                    currentMComm: Option[ModelAPIComm] = None,
                    newItems: List[IDAble] = Nil,
                    structs: List[Struct] = Nil,
                    hiddenIDs: Map[ID, Set[ID]] = Map(), // structID -> structNodeIDs
                    retrievedItems: Map[ID, IDAble] = Map(),
                    expanded: Boolean = false
                  )

  class Backend($: BackendScope[Unit, State]) {

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

    def createItem(kind: String) = {
      val item = ItemKinds.create(kind)
      val modifyState = $.modState(s => s.copy(newItems = item :: s.newItems))
      val notifyBackend = Callback(request(mapi.PutItems(List(item))))
      modifyState >> notifyBackend
    }

    def setCurrentModel(id: ID) = { // TODO dont do anything if already active
      val mcomm = new ModelAPIComm(id)
      val modifyState = $.setState(State(currentMComm = Some(mcomm)))
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

    def toggleAll = $.modState(s => s.copy(expanded = !s.expanded))

    // Option(...).get fine to use in here, since DragNDropMessage knows they are not empty
    def handleDrop(msg: DragNDropMessage) = (msg.drag, msg.drop) match {
      case (DragMessage(idNodeA, None), DropMessage(None, idStructB)) => // new item to struct
        val structB = $.state.map(_.structs.find(_.id == idStructB).get)
        val changeStruct = structB.flatMap(st => replaceStruct(st + StructNode(idNodeA))) // TODO make newItems to be StructNodes?
        val changeNewItems = $.modState(s => s.copy(newItems = s.newItems.filterNot(_.id == idNodeA)))
        changeStruct >> changeNewItems
      case (DragMessage(idNodeA, None), DropMessage(Some(idNodeB), idStructB)) => // new item to node
        val structB = $.state.map(_.structs.find(_.id == idStructB).get)
        val changeStruct = structB.flatMap(st => replaceStruct(st.addTo(idNodeB, Set(StructNode(idNodeA)))))
        val changeNewItems = $.modState(s => s.copy(newItems = s.newItems.filterNot(_.id == idNodeA)))
        changeStruct >> changeNewItems
      case (DragMessage(idNodeA, Some(idStructA)), DropMessage(None, idStructB)) => // old item to struct
        val structA = $.state.map(_.structs.find(_.id == idStructA).get)
        val structB = $.state.map(_.structs.find(_.id == idStructB).get)
        val nodeA = structA.map(_.items.find(_.nodeID == idNodeA).get)
        val nodesToMove = structA.zip(nodeA).map { case (s, sn) => s.getAllChildren(idNodeA) + sn.copy(parent = None) }
        val newStructA = structA.zip(nodesToMove).map { case (s, nodes) => s -- nodes.map(_.nodeID) }
        val newStructB = structB.zip(nodesToMove).map { case (s, nodes) => s ++ nodes }
        newStructA.flatMap(replaceStruct) >> newStructB.flatMap(replaceStruct)
      case (DragMessage(idNodeA, Some(idStructA)), DropMessage(Some(idNodeB), idStructB)) => // old item to node
        val structA = $.state.map(_.structs.find(_.id == idStructA).get)
        val structB = $.state.map(_.structs.find(_.id == idStructB).get)
        val nodeA = structA.map(_.items.find(_.nodeID == idNodeA).get)
        val nodesToMove = structA.zip(nodeA).map { case (s, sn) => s.getAllChildren(idNodeA) + sn.copy(parent = None) }
        val newStructA = structA.zip(nodesToMove).map { case (s, nodes) => s -- nodes.map(_.nodeID) }
        val newStructB = structB.zip(nodesToMove).map { case (s, nodes) => s.addTo(idNodeB, nodes) }
        newStructA.flatMap(replaceStruct) >> newStructB.flatMap(replaceStruct)
    }

    // replaces the Struct with same ID as newStruct with newStruct
    def replaceStruct(newStruct: Struct) = {
      val modifyState = $.modState(s => s.copy(structs = newStruct :: s.structs.filterNot(_.id == newStruct.id)))
      val notifyBackend = sendToModel(mapi.PutItems(List(newStruct)))
      modifyState >> notifyBackend
    }

    def render(s: State) =
      <.div(
        ^.className := Style.outerDiv.htmlClass,
        renderOptionPane,
        renderNewItems(s),
        renderStructs(s)
      )

    def renderOptionPane =
      <.div(
        ^.className := Style.optionPane.htmlClass,
        SPWidgetElements.button(Icon.expand, toggleAll),
        SPWidgetElements.dropdown(
          Icon.plus,
          ItemKinds.list.map(kind => <.div(kind, ^.onClick --> createItem(kind)))
        ),
        ModelChoiceDropdown(id => setCurrentModel(id)),
        SPWidgetElements.TextBox("Filter...", str => filterAllStructs(str))
      )

    def renderNewItems(s: State) =
      <.div(
        ^.className := Style.newItems.htmlClass,
        <.ul(
          <.li("New Items: "),
          s.newItems.toTagMod(idAble => <.li(DraggingTagMod.onDrag(idAble.id, None), ItemKinds.icon(idAble), idAble.name))
        )
      ).when(!s.newItems.isEmpty)

    def renderStructs(s: State) =
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

  val itemExplorerComponent = ScalaComponent.builder[Unit]("ItemExplorer")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply() = SPWidget(spwb => itemExplorerComponent())
}
