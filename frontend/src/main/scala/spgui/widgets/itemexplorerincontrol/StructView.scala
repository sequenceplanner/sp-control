package spgui.widgets.itemexplorerincontrol

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._
import sp.domain.logic.StructLogic._
import spgui.components.DragAndDrop.{DataOnDrag, OnDataDrop}
import spgui.components.Icon

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

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
