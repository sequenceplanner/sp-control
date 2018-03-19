package spgui.widgets.itemexplorerincontrol

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._
import sp.domain.logic.StructLogic._
import sp.models.{APIModel => mapi}
import spgui.SPWidget
import spgui.components.{Icon, SPWidgetElements}
import spgui.communication.APIComm.StreamHelper
import spgui.dragging._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import java.util.UUID
import sp.domain.SPMessage
import spgui.communication._
import sp.domain.Logic._


trait MoveInstruction{}
case class MoveToStruct(struct: UUID) extends MoveInstruction
case class MoveToNode(struct: UUID, node: UUID) extends MoveInstruction
case class DraggedIDAble(idAble: IDAble, model: Option[UUID])
case class DraggedStructNode(node: StructNode, model: Option[UUID])

object ItemExplorer {
  case class State(
    currentMComm: Option[ModelAPIComm] = None,
    currentModelID: Option[UUID] = None,
    newItems: List[(StructNode, IDAble)] = Nil,
    structs: List[Struct] = Nil,
    hiddenIDs: Map[ID, Set[ID]] = Map(), // structID -> structNodeIDs
    expanded: Boolean = false,
    items: Map[ID, IDAble] = Map()
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

    def itemRequest(ids: Set[ID]): Unit = {
      val mcomm = $.state.map(_.currentMComm.get).toFuture
      val futureToRes = mcomm.flatMap(_.request(mapi.GetItems(ids.toList)).takeFirstResponse.map(_._2))
      futureToRes.map {
        case mapi.SPItems(items) => {
          $.modState(s => s.copy(items = s.items ++ items.map(item => item.id -> item))).runNow()
        }
        case _ => Unit
      }
    }

    def createItem(kind: String) = {
      val item = ItemKinds.create(kind)
      val modifyState = $.modState(s => s.copy(newItems = (StructNode(item.id), item) :: s.newItems))
      val notifyBackend = Callback(request(mapi.PutItems(List(item))))
      modifyState >> notifyBackend
    }

    def setCurrentModel(id: ID) = { // TODO dont do anything if already active
      val mcomm = new ModelAPIComm(id)

      val modifyState = $.setState(State(currentMComm = Some(mcomm), currentModelID = Some(id)))
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
    def handleDrop(moveInstruction: MoveInstruction)(msg: DropData): Unit =  {
      //val target = $.state.map(_.structs.find(_.id == msg.dropTarget).get._1)
      //println(msg)
      // val target = $.state.map(_.newItems.find(_._1.nodeID == targetId).get._1).runNow()
      // //val target = $.state.map(_.newItems.find(_.nodeId == targetId).get).runNow()
      // (msg.data, target) match {
      //   case (nodeA: StructNode, nodeB: StructNode) => {}
      // }

      moveInstruction match {
        case m: MoveToStruct => {
          println("move to struct")
          msg.data match {
            case mess: DraggedIDAble => {
              println("TODO")
            }
            case mess: DraggedStructNode => {
              val nodeA = mess.node
              val idNodeA = nodeA.nodeID
              val idStructB = m.struct
              val structB = $.state.map(_.structs.find(_.id == idStructB).get)
              
              // println("b:" + structB.runNow())
              // println("a: " + nodeA)
              val newNode = nodeA.copy(nodeID = UUID.randomUUID())

              // if (modelID != something
              $.state.map{
                s => if(s.currentModelID.get == mess.model.getOrElse(null)) Unit
                else {
                  val externalMComm = new ModelAPIComm(mess.model.get)
                  val req = externalMComm.request(mapi.GetItems(List(nodeA.item)))
                  val items = req.takeFirstResponse.map(_._2).map {
                    case mapi.SPItems(items) => {
                      //println("new items: " + items )
                      sendToModel(mapi.PutItems(items)).runNow()
                      replaceStruct(structB.runNow().copy(items = structB.runNow().items ++ Set(newNode))).runNow()
                      $.modState(s => s.copy(
                        items = s.items ++ items.map(i => Map(i.id -> i)).flatten.toMap
                      )).runNow()
                    }
                    case _ => Set[IDAble]()
                  }
                }
              }.runNow()                     
            }
            case _ => {
              println("something else")
            }
          }
        }
        case m:MoveToNode => {
          println("move to node")
          // val nodeA = $.state.map(_.newItems.find(_._1.nodeID == idNodeA).get._1)
          // val structB = $.state.map(_.structs.find(_.id == idStructB).get)
          // val changeStruct = structB.zip(nodeA).flatMap { case (s, n) => replaceStruct(s.addTo(idNodeB, Set(n))) }
          // val changeNewItems = $.modState(s => s.copy(newItems = s.newItems.filterNot(_._1.nodeID == idNodeA)))
          // changeStruct >> changeNewItems
   
        }
      }
    }

    //}(msg.data, msg.dropTarget/*msg.drag, msg.drop*/) match {
      // case (DragMessage(idNodeA, None), DropMessage(None, idStructB)) => // new item to struct
      //   val nodeA = $.state.map(_.newItems.find(_._1.nodeID == idNodeA).get._1)
      //   val structB = $.state.map(_.structs.find(_.id == idStructB).get)
      //   val changeStruct = structB.zip(nodeA).flatMap { case (s, n) => replaceStruct(s + n) }
      //   val changeNewItems = $.modState(s => s.copy(newItems = s.newItems.filterNot(_._1.nodeID == idNodeA)))
      //   changeStruct >> changeNewItems
      // case (DragMessage(idNodeA, None), DropMessage(Some(idNodeB), idStructB)) => // new item to node
      //   val nodeA = $.state.map(_.newItems.find(_._1.nodeID == idNodeA).get._1)
      //   val structB = $.state.map(_.structs.find(_.id == idStructB).get)
      //   val changeStruct = structB.zip(nodeA).flatMap { case (s, n) => replaceStruct(s.addTo(idNodeB, Set(n))) }
      //   val changeNewItems = $.modState(s => s.copy(newItems = s.newItems.filterNot(_._1.nodeID == idNodeA)))
      //   changeStruct >> changeNewItems
      // case (DragMessage(idNodeA, Some(idStructA)), DropMessage(None, idStructB)) => // old item to struct
      //   val structA = $.state.map(_.structs.find(_.id == idStructA).get)
      //   val structB = $.state.map(_.structs.find(_.id == idStructB).get)
      //   val nodeA = structA.map(_.items.find(_.nodeID == idNodeA).get)
      //   val nodesToMove = structA.zip(nodeA).map { case (s, sn) => s.getAllChildren(idNodeA) + sn.copy(parent = None) }
      //   val newStructA = structA.zip(nodesToMove).map { case (s, nodes) => s -- nodes.map(_.nodeID) }
      //   val newStructB = structB.zip(nodesToMove).map { case (s, nodes) => s ++ nodes }
      //   newStructA.flatMap(replaceStruct) >> newStructB.flatMap(replaceStruct)
      // case (DragMessage(idNodeA, Some(idStructA)), DropMessage(Some(idNodeB), idStructB)) => // old item to node
      //   val structA = $.state.map(_.structs.find(_.id == idStructA).get)
      //   val structB = $.state.map(_.structs.find(_.id == idStructB).get)
      //   val nodeA = structA.map(_.items.find(_.nodeID == idNodeA).get)
      //   val nodesToMove = structA.zip(nodeA).map { case (s, sn) => s.getAllChildren(idNodeA) + sn.copy(parent = None) }
      //   val newStructA = structA.zip(nodesToMove).map { case (s, nodes) => s -- nodes.map(_.nodeID) }
      //   val newStructB = structB.zip(nodesToMove).map { case (s, nodes) => s.addTo(idNodeB, nodes) }
      //   newStructA.flatMap(replaceStruct) >> newStructB.flatMap(replaceStruct)
    
//    def handleDrop(msg: DropData) = {}

    // replaces the Struct with same ID as newStruct with newStruct
    def replaceStruct(newStruct: Struct) = {
      val modifyState = $.modState { s =>
        s.copy(structs = s.structs.map(st => if (st.id == newStruct.id) newStruct else st))
      }
      val notifyBackend = sendToModel(mapi.PutItems(List(newStruct)))
      modifyState >> notifyBackend
    }

    def removeNode(data: DropData) {
      data.data match {
        case DraggedStructNode(struct, modelID) => $.modState { s =>
          s.copy(structs = s.structs.map(st => st.copy(items = st.items.filter(it => it!= struct))))
        }.runNow()
      }
    }

    def render(s: State) =
      <.div(
        ^.className := Style.outerDiv.htmlClass,
        renderOptionPane,
        renderNewItems(s.newItems, s.currentModelID),
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

    def renderNewItems(newItems: List[(StructNode, IDAble)], currentModelID: Option[UUID]) =
      <.div(
        ^.className := Style.newItems.htmlClass,
        <.ul(
          <.li("New Items: ").when(!newItems.isEmpty),
          newItems.toTagMod { case (sn, idAble) =>
            <.li(
              SPWidgetElements.draggable(idAble.name, DraggedIDAble(idAble, currentModelID), "todo", (d:DropData) => println("yes" + d)),
              ItemKinds.icon(idAble), idAble.name)
          }
        )
      )//.when(!newItems.isEmpty) this somehow causes StructViews to be rerendered upon newItems-change

    def renderStructs(s: State) =
      <.div(
        ^.className := Style.structsView.htmlClass,
        <.ul(
          ^.className := Style.ul.htmlClass, 
          s.structs.toTagMod { struct =>
            <.li(
              StructView(
                struct,
                items = s.items,
                retrieveItems = Some(itemRequest),
                handleDrop = Some(handleDrop),
                handleDragged = (data:DropData) => removeNode(data),
                filteredNodes = s.hiddenIDs(struct.id),
                expanded = s.expanded,
                modelID = s.currentModelID
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




