package spgui.widgets.itemexplorerincontrol

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain.logic.AttributeLogic._
import sp.domain.logic.StructLogic._
// import spgui.components.DragAndDrop.{DataOnDrag, OnDataDrop}
import spgui.components.Icon
import spgui.components.SPWidgetElements
import spgui.dragging._
import java.util.UUID

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

import spgui.communication._
import sp.models.{APIModel => mapi}
import sp.domain.SPMessage
//import sp.domain.Logic._
// import sp.domain.SPAttributes._
// import sp.domain.SPValue
import sp.domain._

object StructView {
  case class Props(
                    struct: Struct,
                    items: Map[ID, IDAble],
                    retrieveItems: Option[Set[ID] => Unit],
                    handleDrop: Option[DragDropData => Unit] = None,
                    handleDragged: Option[DragDropData => Unit] = None,
                    filteredNodes: Set[ID],
                    expanded: Boolean,
                    modelID: Option[UUID] = None
                  )
  case class State(
                    expandedNodes: Set[ID] = Set() 
                  )

  class Backend($: BackendScope[Props, State]) {
    def toggle(id: ID, childrenIDs: Set[ID]) = {
      val modNodes = $.modState(s => s.copy(expandedNodes = s.expandedNodes + id -- s.expandedNodes.intersect(Set(id))))
      retrieveItems(childrenIDs) >> modNodes
    }

    def toggleAll() = {
      def toggleRecursive(id: ID, struct: Struct): Unit = {
        val directChildren = struct.getChildren(id).map(_.item)
        val directChildrenNodes = struct.getChildren(id).map(_.nodeID)
        toggle(id, directChildren).runNow()
        directChildrenNodes.map{ c => toggleRecursive(c, struct) }
      }

      $.props.map{ p =>
        val directChildren = p.struct.items.filter(_.parent.isEmpty).map(_.item)
        val directChildrenNodes = p.struct.items.filter(_.parent.isEmpty).map(_.nodeID)
        toggle(p.struct.id, directChildren).runNow()
        directChildrenNodes.map{ c => toggleRecursive(c, p.struct)}
      }
    }

    def retrieveItems(ids: Set[ID]) = {
      $.props.map(p => p.retrieveItems.get(ids))
    }

    def render(p: Props, s: State) = {
      val rootItemsToRender = p.struct.items.filter(sn => sn.parent.isEmpty && !p.filteredNodes.contains(sn.nodeID))
      lazy val directChildren = p.struct.items.filter(_.parent.isEmpty).map(_.item)
      <.div(
        SPWidgetElements.DragoverZoneWithChild(
          p.handleDrop.get,
          DroppedOnStruct(p.struct, p.modelID.get),
          <.div(
            ^.className := Style.nodeOuter.htmlClass,
            if(directChildren.isEmpty) EmptyVdom
            else {
              <.span(
                 if (s.expandedNodes.contains(p.struct.id)) Icon.toggleDown else Icon.toggleRight,
                ^.onClick --> toggle(p.struct.id, directChildren)
              )
            },
            Icon.folder,
            p.struct.name
          )
        ),
        <.ul(
          ^.className := Style.ul.htmlClass,
          rootItemsToRender.toTagMod(node => <.li(renderNode(node, p, s)))
        ).when(s.expandedNodes.contains(p.struct.id))
      )
    }

    def renderNode(node: StructNode, p: Props, s: State): TagMod = {
      val childrenToRender = p.struct.getChildren(node.nodeID).filterNot(sn => p.filteredNodes.contains(sn.nodeID))
      lazy val directChildren = p.struct.getChildren(node.nodeID).map(_.item)
      val arrowIcon = if (s.expandedNodes.contains(node.nodeID)) Icon.toggleDown else Icon.toggleRight
      <.div(
        SPWidgetElements.DragoverZoneWithChild(
          p.handleDrop.get,
          DroppedOnNode(Some(p.struct), node, p.modelID.get),
          <.div(
            ^.className := Style.nodeOuter.htmlClass,
            if(childrenToRender.isEmpty) EmptyVdom
            else {
              <.span(
                arrowIcon,
                ^.onClick --> toggle(node.nodeID, directChildren)
              )
            },
            <.span(renderNodeItem(node, p, s),
              SPWidgetElements.draggable(p.struct.name, DraggedStructNode(Some(p.struct), node, p.modelID), "todo", p.handleDragged.get),
            )
          )
        ),
        <.ul(
          ^.className := Style.ul.htmlClass,
          childrenToRender.toTagMod(sn => <.li(renderNode(sn, p, s)))
        ).when(s.expandedNodes.contains(node.nodeID))
      )
    }

    def renderNodeItem(node: StructNode, p: Props, s: State): TagMod = {      
      val arrowIcon = if (s.expandedNodes.contains(node.nodeID)) Icon.toggleRight else Icon.toggleDown
      val itemOp = p.items.get(node.item)
      val itemIcon = itemOp.map(ItemKinds.icon).getOrElse(Icon.question)
      val shownName = itemOp.map(_.name).getOrElse({
        node.item.toString
      })
      <.div(
        itemIcon,
        shownName
      )
    }
  }

  val component = ScalaComponent.builder[Props]("StructView")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillReceiveProps { scope =>
      val nextExpanded = scope.nextProps.expanded
      val expandedChanged = scope.currentProps.expanded != nextExpanded
      if (expandedChanged) {
        scope.backend.toggleAll() >>
        {
          if (nextExpanded) {
            scope.backend.retrieveItems(scope.nextProps.items.keySet)
          } else {
            scope.modState(_.copy(expandedNodes = Set()))
          }
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
             retrieveItems: Option[Set[ID] => Unit] = None,
             handleDrop: Option[DragDropData => Unit] = None,
             handleDragged: Option[DragDropData => Unit] = None,
             filteredNodes: Set[ID] = Set(),
             expanded: Boolean = false,
             modelID: Option[UUID] = None
           ) = component(Props(struct, items, retrieveItems, handleDrop, handleDragged, filteredNodes, expanded, modelID))
}
