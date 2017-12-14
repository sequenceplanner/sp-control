package spgui.widgets.itemexplorerincontrol

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._
import sp.domain.logic.StructLogic._
import sp.models.{APIModel => mapi}
import spgui.{SPWidget, SPWidgetBase}
import spgui.components.DragAndDrop.DataOnDrag
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
      $.modState(s => s.copy(newItems = item :: s.newItems))
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
          s.newItems.toTagMod(idAble => <.li(DataOnDrag(idAble.id.toString), ItemKinds.icon(idAble), idAble.name))
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
