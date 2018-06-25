package spgui.widgets.sopmaker

import java.util.UUID
import japgolly.scalajs.react._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.all.svg

import spgui.communication._
import sp.domain._
import scalacss.ScalaCssReact._
import scala.scalajs.js
import spgui.components.SPWidgetElements


import spgui.dragging._
import spgui.circuit._

import spgui.circuit.{ SPGUICircuit, UpdateGlobalState, GlobalState }
import spgui.{SPWidget, SPWidgetBase}
import spgui.components.Icon

// sealed trait RenderNode {
//   val nodeId: UUID
//   val w: Float
//   val h: Float
// }

// sealed trait RenderGroup extends RenderNode {
//   val children: List[RenderNode]
// }

// case class RenderParallel(
//   nodeId: UUID, w: Float, h:Float, children: List[RenderNode]) extends RenderGroup
// case class RenderAlternative(
//   nodeId: UUID, w: Float, h:Float, children: List[RenderNode]) extends RenderGroup
// case class RenderArbitrary(
//   nodeId: UUID, w: Float, h:Float, children: List[RenderNode]) extends RenderGroup
// case class RenderSometimeSequencenode(
//   nodeId: UUID, w: Float, h:Float, children: List[RenderNode]) extends RenderGroup
// case class RenderOther(
//   nodeId: UUID, w: Float, h:Float, children: List[RenderNode]) extends RenderGroup
// case class RenderSequence(
//   nodeId: UUID, w: Float, h:Float, children: List[RenderSequenceElement]) extends RenderGroup
// case class RenderSequenceElement(
//   nodeId: UUID, w: Float, h:Float, self: RenderNode) extends RenderNode
// case class RenderOperationNode(
//   nodeId: UUID, w:Float, h:Float, sop: OperationNode) extends RenderNode

// case class DraggedSOP(sop: SOP) extends DragData
// case class DroppedOnSOP(sop: Any = Unit) extends DropData

object SopMakerWidget {

  var dropZones: scala.collection.mutable.Map[UUID, (UUID, SopVisualiser.DropzoneDirection.Value)] =
    scala.collection.mutable.Map()

  val newOpId = UUID.randomUUID()
  val newParallelId = UUID.randomUUID()
  val newArbitraryId = UUID.randomUUID()
  val newAlternativeId = UUID.randomUUID()

  case class State(sop: SOP)

  val idm = ExampleSops.ops.map(o => o.id -> o).toMap
 
  private class Backend($: BackendScope[Unit, State]) {
    def render(state: State) = {
      <.div(
        SPWidgetElements.buttonGroup(
          Seq(
            SPWidgetElements.button(Icon.save, Callback(println("TODO"))),
            SPWidgetElements.TextBox("save as...", (s) => Callback(println(s))),
            SPWidgetElements.dropdown("load SOP", Seq(
              SPWidgetElements.dropdownElement("test",Callback(println("TODO"))),
              SPWidgetElements.dropdownElement("test",Callback(println("TODO"))),
              SPWidgetElements.dropdownElement("test",Callback(println("TODO")))
            )),
            SopMakerGraphics.menuOp("OP", newOpId)
          )
        ),
        SopVisualiser(state.sop, ExampleSops.ops, Map(), Some(onDropEvent), Some(onDragEvent))
      )
    }

    def handleMouseOver(style:String)(e: ReactMouseEvent) = Callback {
      Dragging.setDraggingStyle(style)
    }

    def op(opId: UUID, opname: String, x: Float, y: Float): TagMod = {
      <.span(
        ^.draggable := false,
        SPWidgetElements.draggable(opname, DraggedSOP(findSop(opId)), "sop", (d:DragDropData) => println("dropped an op")),
        SopMakerGraphics.op(opname, x.toInt, y.toInt)
      )
    }


    def onDropEvent(id: UUID, direction: SopVisualiser.DropzoneDirection.Value)(e: DragDropData): Unit = {
      e.dragData match {
        case DraggedSOP(sop: SOP) => {
          val sopId = id
          $.modState(
            s => State(insertSop(s.sop, sopId, cloneSop(sop), direction ))
          ).runNow()
        }
        case _ => Unit
      }
    }

    def onDragEvent(e:DragDropData) = {
      e.dropData match {
        case DroppedOnSOP(_) => {
          e.dragData match {
            case DraggedSOP(sop:SOP) => {
              $.modState(
                s => State(deleteSop(s.sop, sop.nodeID))
              ).runNow()
            }
          }
        }
        case _ => Unit
      }
    }

    def onMount() = Callback{
    }

    def onUnmount() = Callback {
    }

    def sopList(root: SOP): List[SOP] = {
      root :: root.sop.map(e => sopList(e)).toList.flatten
    }

    def findSop(sopId: UUID): SOP = {
      val root = $.state.runNow().sop
      sopList(root).filter(x => x.nodeID == sopId).head
    }

    def cloneSop(sop: SOP): SOP = {
      sop match {
        case r: Parallel => r.copy(nodeID = UUID.randomUUID())
        case r: Sequence => r.copy(nodeID = UUID.randomUUID())
        case r: OperationNode => r.copy(nodeID = UUID.randomUUID())
      }
    }

    def insertSop(root: SOP, targetId: UUID, sop: SOP, direction: SopVisualiser.DropzoneDirection.Value): SOP = {
      if(root.nodeID == targetId) {
        root match {
          case r: Parallel => {
            direction match {
              case SopVisualiser.DropzoneDirection.Left => Parallel(sop = cloneSop(sop):: r.sop)
              case SopVisualiser.DropzoneDirection.Right => Parallel(sop = r.sop :+ cloneSop(sop))
              case SopVisualiser.DropzoneDirection.Up => Sequence(sop = List(cloneSop(sop), r))
              case SopVisualiser.DropzoneDirection.Down => Sequence(sop = List(r, cloneSop(sop)))
            }
          }
          case r: Sequence => Sequence(sop = cloneSop(sop) :: r.sop)
          case r: OperationNode => {
            direction match {
              case SopVisualiser.DropzoneDirection.Left => Parallel(sop = List(cloneSop(sop), r))
              case SopVisualiser.DropzoneDirection.Right => Parallel(sop = List(r, cloneSop(sop)))
              case SopVisualiser.DropzoneDirection.Up => Sequence(sop = List(cloneSop(sop), r))
              case SopVisualiser.DropzoneDirection.Down => Sequence(sop = List(r, cloneSop(sop)))
            }
          }
        }
      } else {
        root match {
          case r: Parallel =>
            Parallel(sop = r.sop.collect{case e => insertSop(e, targetId, sop, direction)})
          case r: Sequence =>
            Sequence(sop = r.sop.collect{case e => insertSop(e, targetId, sop, direction)})
          case r: OperationNode => r // TODO
        }
      }
    }
  }

  def deleteSop(root: SOP, targetId: UUID): SOP = {
    root match {
      case r: Parallel => {
        r.copy(sop = r.sop.filter(_.nodeID != targetId).map(deleteSop(_, targetId)))
      }

      case r: Sequence => {
        r.copy(sop = r.sop.filter(_.nodeID != targetId).map(deleteSop(_, targetId)))
      }

      case r: OperationNode => {
        r.copy(sop = r.sop.filter(_.nodeID != targetId).map(deleteSop(_, targetId)))
      }
    }
  }

  private val component = ScalaComponent.builder[Unit]("SopMakerWidget")
    .initialState(State(sop = ExampleSops.randoSop))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .componentDidMount(_.backend.onMount())
    .build

  def apply() = spgui.SPWidget(spwb => component())
}
