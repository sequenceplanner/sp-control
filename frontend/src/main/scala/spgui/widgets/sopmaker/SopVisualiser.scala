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

object SopVisualiser {
  object DropzoneDirection extends Enumeration {
    val Left, Right, Up, Down = Value
  }

  val parallelBarHeight = 12f
  val opHorizontalBarOffset = 12f
  val opVerticalBarOffset = 12f
  val opHeight = 80f
  val opWidth = 120f
  val opSpacingX = 10f
  val opSpacingY = 25f
  val opSpacingYInsideGroup = 10
  val opSpacingXInsideGroup = 35f

  val paddingTop = 40f
  val paddingLeft = 40f

  case class Props(
    sop: SOP,
    ops: List[Operation],
    opStates: Map[ID, SPValue] = Map(),
    onDropEvent: Option[(UUID, DropzoneDirection.Value) => (DragDropData) => Unit] = None,
    onDragEvent: Option[(DragDropData) => Unit] = None
  )

  class Backend($: BackendScope[Props, Unit]) {
    def render(p: Props, s: Unit) = {
      <.span(
        ^.onMouseOver ==> handleMouseOver("sop_style"),
        ^.onMouseOut ==> handleMouseOver("not_sop_style"),
        SopMakerCSS.sopContainer,
        getRenderTree(
          traverseTree( p.sop ),
          getTreeWidth( p.sop ) * 0.5f + paddingLeft,
          paddingTop
        ).toTagMod
      )
    }

    def getRenderTree(node: RenderNode, xOffset: Float, yOffset: Float): List[TagMod] = {
      val isInteractive = !$.props.runNow().onDropEvent.isEmpty
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
          {
            if(isInteractive) {
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
              )
            } else {
              List()
            }
          } ++
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
          val ops = $.props.runNow().ops.map(o => o.id -> o).toMap
          val opname = ops.get(n.sop.operation).map(_.name).getOrElse("[unknown op]")
          val opState =
            $.props.runNow().opStates.get(node.nodeId).getOrElse(SPValue("NoneState")).toString

          List(op(n.sop.nodeID, opname, xOffset, yOffset, opState)) ++
          {
            if(isInteractive) {
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
            } else List()
          }
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
          nodeId = s.operation,
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

    def sopList(root: SOP): List[SOP] = {
      root :: root.sop.map(e => sopList(e)).toList.flatten
    }

    def findSop(sopId: UUID): SOP = {
      val root = $.props.runNow().sop
      sopList(root).filter(x => x.nodeID == sopId).head
    }

    def op(opId: UUID, opname: String, x: Float, y: Float, state: String): TagMod = {
      val onDragEvent = $.props.runNow().onDragEvent
      <.span(
        ^.draggable := false,
        {
          if(!onDragEvent.isEmpty) SPWidgetElements.draggable(opname, DraggedSOP(findSop(opId)), "sop", onDragEvent.get)
          else EmptyVdom
        },
        SopMakerGraphics.op(opname, x.toInt, y.toInt, state)
      )
    }

    def dropZone(
      direction: DropzoneDirection.Value, sop:Any, id: UUID, x: Float, y: Float, w: Float, h: Float): TagMod =
    {
      println("making a dropzone " + id.toString)
      SPWidgetElements.DragoverZoneRect(
        $.props.runNow().onDropEvent.get(id, direction), DroppedOnSOP(sop), x, y, w, h)
    }
    
    def handleMouseOver(style:String)(e: ReactMouseEvent) = Callback {
      Dragging.setDraggingStyle(style)
    }
  }

  private val component = ScalaComponent.builder[Props]("SopVisualiser")
    .renderBackend[Backend]
    .build

  // onDropevent and onDragevent can be excuded to create a noninteractive component
  def apply(
    sop: SOP,
    ops: List[Operation],
    opStates: Map[ID, SPValue] = Map(), 
    onDropEvent: Option[(UUID, DropzoneDirection.Value) => (DragDropData) => Unit] = None,
    onDragEvent: Option[(DragDropData) => Unit] = None
  ) = component(Props(sop, ops, opStates, onDropEvent, onDragEvent))
}
