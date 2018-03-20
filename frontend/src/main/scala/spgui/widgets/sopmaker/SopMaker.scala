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

sealed trait RenderNode {
  val nodeId: UUID
  val w: Float
  val h: Float
}

sealed trait RenderGroup extends RenderNode {
  val children: List[RenderNode]
}

case class RenderParallel(
  nodeId: UUID, w: Float, h:Float, children: List[RenderNode]) extends RenderGroup
case class RenderAlternative(
  nodeId: UUID, w: Float, h:Float, children: List[RenderNode]) extends RenderGroup
case class RenderArbitrary(
  nodeId: UUID, w: Float, h:Float, children: List[RenderNode]) extends RenderGroup
case class RenderSometimeSequencenode(
  nodeId: UUID, w: Float, h:Float, children: List[RenderNode]) extends RenderGroup
case class RenderOther(
  nodeId: UUID, w: Float, h:Float, children: List[RenderNode]) extends RenderGroup
case class RenderSequence(
  nodeId: UUID, w: Float, h:Float, children: List[RenderSequenceElement]) extends RenderGroup
case class RenderSequenceElement(
  nodeId: UUID, w: Float, h:Float, self: RenderNode) extends RenderNode
case class RenderOperationNode(
  nodeId: UUID, w:Float, h:Float, sop: OperationNode) extends RenderNode

case class DraggedSOP(sop: SOP) extends DragData
case class DroppedOnSOP(sop: Any = Unit) extends DropData

object SopMakerWidget {
  object DropzoneDirection extends Enumeration {
    val Left, Right, Up, Down = Value
  }

  var dropZones: scala.collection.mutable.Map[UUID, (UUID, DropzoneDirection.Value)] =
    scala.collection.mutable.Map()

  val parallelBarHeight = 12f

  val opHorizontalBarOffset = 12f
  val opVerticalBarOffset = 12f
  val opHeight = 80f
  val opWidth = 120f
  val opSpacingX = 10f
  val opSpacingY = 25f
  val opSpacingYInsideGroup = 10
  val opSpacingXInsideGroup = 35f

  val newOpId = UUID.randomUUID()
  val newParallelId = UUID.randomUUID()
  val newArbitraryId = UUID.randomUUID()
  val newAlternativeId = UUID.randomUUID()

  var xOrigin = 0f
  var yOrigin = 0f

  case class State(sop: SOP)

  val idm = ExampleSops.ops.map(o=>o.id -> o).toMap

  private class Backend($: BackendScope[Unit, State]) {
    /*
     val eventHandler = BackendCommunication.getMessageObserver(
     mess => {
     println("[SopMaker] Got event " + mess.toString)
     },
     "events"
     )
     */
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
            SopMakerGraphics.menuOp("OP", newOpId),
            SopMakerGraphics.menuOp("||", newAlternativeId),
            SopMakerGraphics.menuOp("&&", newParallelId),
            SopMakerGraphics.menuOp("??", newArbitraryId)
          )
        ),
        <.span(
          ^.onMouseOver ==> handleMouseOver("sop_style"),
          ^.onMouseOut ==> handleMouseOver("not_sop_style"),
          SopMakerCSS.sopContainer,
          getRenderTree(
            traverseTree( state.sop ),
            getTreeWidth( state.sop ) * 0.5f + paddingLeft,
            paddingTop
          ).toTagMod
        ))
    }

    def handleMouseOver(style:String)(e: ReactMouseEvent) = Callback {
      Dragging.setDraggingStyle(style)
    }

    val paddingTop = 40f
    val paddingLeft = 40f

    def op(opId: UUID, opname: String, x: Float, y: Float): TagMod = {
      <.span(
        ^.draggable := false,
        SPWidgetElements.draggable(opname, DraggedSOP(findSop(opId)), "sop", (d:DragDropData) => println("dropped an op")),
        SopMakerGraphics.sop(opname, x.toInt, y.toInt)
      )
    }

    def dropZone(
      direction: DropzoneDirection.Value, sop:Any, id: UUID, x: Float, y: Float, w: Float, h: Float): TagMod =
    {
      println("making a dropzone " + id.toString)
      SPWidgetElements.DragoverZoneRect(onDropEvent(id, direction), DroppedOnSOP(sop), x, y, w, h)
    }
    def getRenderTree(node: RenderNode, xOffset: Float, yOffset: Float): List[TagMod] = {
      println(node.nodeId.toString)
      node match {
        case n: RenderParallel => {
          var w = 0f
          var children = List[TagMod]()
          for(e <- n.children) {
            val child = getRenderTree(
              e,
              xOffset + w + e.w/2f - n.w/2f + opSpacingX + opSpacingXInsideGroup,
              yOffset + parallelBarHeight + opSpacingY + opSpacingYInsideGroup
            )
            children = children ++ child ++ List(
              SopMakerGraphics.sopConnectionLine(
                xOffset + w + e.w/2f - n.w/2f + opSpacingX/2f + opSpacingXInsideGroup,
                yOffset + parallelBarHeight,
                opSpacingY + opSpacingYInsideGroup
              ),
              SopMakerGraphics.sopConnectionLine(
                xOffset + w + e.w/2f - n.w/2f + opSpacingX/2f + opSpacingXInsideGroup,
                yOffset + parallelBarHeight + opSpacingY + opSpacingYInsideGroup + e.h,
                n.h - e.h - 2*parallelBarHeight - opSpacingYInsideGroup - opSpacingY
              )
            )
            w += e.w
          }
          List(
            dropZone(   // Left dropzone
              direction = DropzoneDirection.Left,
              sop = DroppedOnSOP(),
              id = n.nodeId,
              x = xOffset - n.w/2 + opWidth/2,
              y = yOffset,
              w = opSpacingXInsideGroup,
              h = n.h - parallelBarHeight
            ),
            dropZone(   // Right dropzone
              direction = DropzoneDirection.Right,
              id = n.nodeId,
              sop = DroppedOnSOP(),
              x = xOffset + n.w/2 - opSpacingXInsideGroup - opSpacingX + opWidth/2,
              y = yOffset,
              w = opSpacingXInsideGroup,
              h = n.h - parallelBarHeight
            ),
            dropZone(   // Top dropzone
              direction = DropzoneDirection.Up,
              sop = DroppedOnSOP(),
              id = n.nodeId,
              x = xOffset - n.w/2 + opWidth/2,
              y = yOffset,
              w = n.w,
              h = parallelBarHeight
            ),
            dropZone(   // Bottom dropzone
              direction = DropzoneDirection.Down,
              sop = DroppedOnSOP(),
              id = n.nodeId,
              x = xOffset - n.w/2 + opWidth/2,
              y = yOffset + n.h - parallelBarHeight,
              w = n.w,
              h = parallelBarHeight
            )
          ) ++
          List(
            SopMakerGraphics.parallelBars(xOffset - n.w/2, yOffset,n.w- opSpacingX)) ++
          children ++
          List(SopMakerGraphics.parallelBars(
            xOffset - n.w/2,
            yOffset + n.h - parallelBarHeight,
            n.w - opSpacingX
          ))
        }
        case n: RenderSequence =>  getRenderSequence(n.children, xOffset, yOffset)
          
        case n: RenderOperationNode => {
          val opname = idm.get(n.sop.operation).map(_.name).getOrElse("[unknown op]")

          List(op(n.sop.nodeID, opname, xOffset, yOffset)) ++
          List(
            dropZone(
              direction = DropzoneDirection.Left,
              id = n.nodeId,
              sop = DroppedOnSOP(),
              x = xOffset,
              y = yOffset + opHorizontalBarOffset,
              w = opVerticalBarOffset,
              h = opHeight - 2*opHorizontalBarOffset
            ),
            dropZone(
              direction = DropzoneDirection.Right,
              sop = DroppedOnSOP(),
              id = n.nodeId,
              x = xOffset + opWidth - opVerticalBarOffset,
              y = yOffset + opHorizontalBarOffset,
              w = opVerticalBarOffset,
              h = opHeight - 2*opHorizontalBarOffset
            ),
            dropZone(
              direction = DropzoneDirection.Up,
              id = n.nodeId,
              sop = DroppedOnSOP(),
              x = xOffset,
              y = yOffset,
              w = opWidth,
              h = opHorizontalBarOffset
            ),
            dropZone(
              direction = DropzoneDirection.Down,
              sop = DroppedOnSOP(),
              id = n.nodeId,
              x = xOffset,
              y = yOffset + opHeight - opVerticalBarOffset,
              w = opWidth,
              h = opVerticalBarOffset
            )
          )
        }
      }
    }

    def getRenderSequence(children: List[RenderSequenceElement], xOffset: Float, yOffset:Float ): List[TagMod] = {
      val head = children.head
      val tail = children.tail
      tail match {
        case Nil => {
          getRenderTree( head.self, xOffset, yOffset )
        }
        case _ =>
          getRenderTree( head.self, xOffset, yOffset ) ++
          List(SopMakerGraphics.sopConnectionLine(
            xOffset,
            yOffset + head.h,
            opSpacingY
          )) ++
          getRenderSequence(tail, xOffset, yOffset + head.h + opSpacingY )
      }
    }

    def traverseTree(sop: SOP): RenderNode = {
      sop match {
        case s: Parallel => RenderParallel(
          nodeId = s.nodeID,
          w = getTreeWidth(s),
          h = getTreeHeight(s),
          children = sop.sop.collect{case e => traverseTree(e)}
        )
        case s: Sequence => traverseSequence(s)
        case s: OperationNode => RenderOperationNode(
          nodeId = s.nodeID,
          w = getTreeWidth(s),
          h = getTreeHeight(s),
          sop = s
        )
      }
    }

    def traverseSequence(s: Sequence): RenderSequence = {
      if(s.sop.isEmpty) null else RenderSequence(
        nodeId = s.nodeID,
        h = getTreeHeight(s),
        w = getTreeWidth(s),
        children = s.sop.collect{case e: SOP => {
          RenderSequenceElement(
            nodeId = e.nodeID,
            getTreeWidth(e),
            getTreeHeight(e),
            traverseTree(e)
          )
        }}
      )
    }

    def getTreeWidth(sop: SOP): Float = {
      sop match {
        // groups are as wide as the sum of all children widths + its own padding
        case s: Parallel => s.sop.map(e => getTreeWidth(e)).sum + 2*opSpacingX + opSpacingXInsideGroup *2 + opSpacingX
        case s: Sequence => { // sequences are as wide as their widest elements
          if(s.sop.isEmpty) 0
          else math.max(getTreeWidth(s.sop.head), getTreeWidth(Sequence(s.sop.tail)))
        }
        case s: OperationNode => {
          opWidth + opSpacingX
        }
      }
    }

    def getTreeHeight(sop: SOP): Float = {
      sop match  {
        case s: Parallel => {
          if(s.sop.isEmpty) 0
          else math.max(
            getTreeHeight(s.sop.head) + (2*parallelBarHeight + 2*opSpacingY + 2*opSpacingYInsideGroup),
            getTreeHeight(Parallel(s.sop.tail))
          )
        }
        case s: Sequence => {
          s.sop.map(e => getTreeHeight(e)).foldLeft(opSpacingY)(_ + _)
        }
        case s: OperationNode => opHeight
      }   
    }

    def onDropEvent(id: UUID, direction: DropzoneDirection.Value)(e: DragDropData): Unit = {
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

    def insertSop(root: SOP, targetId: UUID, sop: SOP, direction: DropzoneDirection.Value): SOP = {
      if(root.nodeID == targetId) {
        root match {
          case r: Parallel => {
            direction match {
              case DropzoneDirection.Left => Parallel(sop = cloneSop(sop):: r.sop)
              case DropzoneDirection.Right => Parallel(sop = r.sop :+ cloneSop(sop))
              case DropzoneDirection.Up => Sequence(sop = List(cloneSop(sop), r))
              case DropzoneDirection.Down => Sequence(sop = List(r, cloneSop(sop)))
            }
          }
          case r: Sequence => Sequence(sop = cloneSop(sop) :: r.sop)
          case r: OperationNode => {
            direction match {
              case DropzoneDirection.Left => Parallel(sop = List(cloneSop(sop), r))
              case DropzoneDirection.Right => Parallel(sop = List(r, cloneSop(sop)))
              case DropzoneDirection.Up => Sequence(sop = List(cloneSop(sop), r))
              case DropzoneDirection.Down => Sequence(sop = List(r, cloneSop(sop)))
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
  
  private val component = ScalaComponent.builder[Unit]("SopMakerWidget")
    .initialState(State(sop = ExampleSops.tinySop))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .componentDidMount(_.backend.onMount()) 
    .build

  def apply() = spgui.SPWidget(spwb => component())
}
