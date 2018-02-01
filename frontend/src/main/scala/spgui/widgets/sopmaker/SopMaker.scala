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
import spgui.components.SPWidgetElementsHmm


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
  val opSpacingY = 10f
  val opSpacingYInsideGroup = 10f
  val opSpacingXInsideGroup = 23f

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
        SPWidgetElements.draggable(opname, opId, "sop"),
        SopMakerGraphics.sop(opname, x.toInt, y.toInt)
      )
    }

    def dropZone(id: UUID, x: Float, y: Float, w: Float, h: Float): TagMod =
      SPWidgetElementsHmm.DragoverZone(id, x, y, w, h)

    def getRenderTree(node: RenderNode, xOffset: Float, yOffset: Float): List[TagMod] = {
      node match {
        case n: RenderParallel => {
          var w = 0f
          var children = List[TagMod]()
          for(e <- n.children) {
            val child = getRenderTree(
              e,
              xOffset + w + e.w/2 - n.w/2 + opSpacingX + opSpacingXInsideGroup,
              yOffset + parallelBarHeight + opSpacingY + opSpacingYInsideGroup
            )
            w += e.w
            children = children ++ child
          }
          val dropzoneLeft  = addDropSubscriber(n.nodeId, DropzoneDirection.Left)
          val dropzoneRight = addDropSubscriber(n.nodeId, DropzoneDirection.Right)
          val dropzoneUp    = addDropSubscriber(n.nodeId, DropzoneDirection.Up)
          val dropzoneDown  = addDropSubscriber(n.nodeId, DropzoneDirection.Down)

          List(
            dropZone(   // Left dropzone
              id = dropzoneLeft,
              x = xOffset - n.w/2 + opWidth/2,
              y = yOffset,
              w = opSpacingXInsideGroup,
              h = n.h - parallelBarHeight
            ),
            dropZone(   // Right dropzone
              id = dropzoneRight,
              x = xOffset + n.w/2 - opSpacingXInsideGroup - opSpacingX + opWidth/2,
              y = yOffset,
              w = opSpacingXInsideGroup,
              h = n.h - parallelBarHeight
            ),
            dropZone(   // Top dropzone
              id = dropzoneUp,
              x = xOffset - n.w/2 + opWidth/2,
              y = yOffset,
              w = n.w,
              h = parallelBarHeight
            ),
            dropZone(   // Bottom dropzone
              id = dropzoneDown,
              x = xOffset - n.w/2 + opWidth/2,
              y = yOffset + n.h - 2*parallelBarHeight,
              w = n.w,
              h = parallelBarHeight
            )
          ) ++
          List(
            SopMakerGraphics.parallelBars(xOffset - n.w/2, yOffset,n.w- opSpacingX)) ++
          children ++
          List(SopMakerGraphics.parallelBars(
            xOffset - n.w/2,
            yOffset + n.h - 2*parallelBarHeight,
            n.w - opSpacingX
          ))
        }
        case n: RenderSequence =>  getRenderSequence(n, xOffset, yOffset)
          
        case n: RenderOperationNode => {
          val opname = idm.get(n.sop.operation).map(_.name).getOrElse("[unknown op]")
          val dropzoneLeft  = addDropSubscriber(n.nodeId, DropzoneDirection.Left)
          val dropzoneRight = addDropSubscriber(n.nodeId, DropzoneDirection.Right)
          val dropzoneUp    = addDropSubscriber(n.nodeId, DropzoneDirection.Up)
          val dropzoneDown  = addDropSubscriber(n.nodeId, DropzoneDirection.Down)

          List(op(n.sop.nodeID, opname, xOffset, yOffset)) ++
          List(
            dropZone(
              id = dropzoneLeft,
              x = xOffset,
              y = yOffset + opHorizontalBarOffset,
              w = opVerticalBarOffset,
              h = opHeight - 2*opHorizontalBarOffset
            ),
            dropZone(
              id = dropzoneRight,
              x = xOffset + opWidth - opVerticalBarOffset,
              y = yOffset + opHorizontalBarOffset,
              w = opVerticalBarOffset,
              h = opHeight - 2*opHorizontalBarOffset
            ),
            dropZone(
              id = dropzoneUp,
              x = xOffset,
              y = yOffset,
              w = opWidth,
              h = opHorizontalBarOffset
            ),
            dropZone(
              id = dropzoneDown,
              x = xOffset,
              y = yOffset + opHeight - opVerticalBarOffset,
              w = opWidth,
              h = opVerticalBarOffset
            )
          )
        }
      }
    }

    def addDropSubscriber(sopID: UUID, direction: DropzoneDirection.Value): UUID = {
      val dropID = UUID.randomUUID()
      dropZones += (dropID -> (sopID, direction))
      dropID
    }

    def removeDropSubscriber(dropID:UUID) = { dropZones -= dropID } 

    def getRenderSequence(seq: RenderSequence, xOffset: Float, yOffset: Float): List[TagMod] = {
      var h = yOffset
      var children = List[TagMod]()
      for(q <- seq.children){
        h += q.h
        children = children ++ getRenderTree( q.self, xOffset, h - q.h )
      }
      children
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
            getTreeHeight(s.sop.head) + (parallelBarHeight*2 + opSpacingY*2 + 2*opSpacingYInsideGroup),
            getTreeHeight(Parallel(s.sop.tail))
          )
        }
        case s: Sequence => {
          s.sop.map(e => getTreeHeight(e)).foldLeft(0f)(_ + _)
        }
        case s: OperationNode => opHeight + opSpacingY
      }   
    }
  
    def onMount() = Callback{
      SPGUICircuit.subscribe(SPGUICircuit.zoom(z => z.draggingState.latestDropEvent.get)){
        e => onDropEvent(e.value)
      }
    }

    def onDropEvent(e:DropEventData): Unit = {
      val sopId = dropZones(e.targetId)._1
      val direction = dropZones(e.targetId)._2
      println(direction)

      $.modState(
        s => State(insertSop(s.sop, sopId, e.droppedId, direction ))
      )
    }.runNow()




    def onUnmount() = Callback {
      println("Unmounting sopmaker")
    }

    def sopList(root: SOP): List[SOP] = {
      root :: root.sop.map(e => sopList(e)).toList.flatten
    }

    def findSop(root: SOP, sopId: UUID): SOP = {
      sopList(root).filter(x => x.nodeID == sopId).head
    }

    def cloneSop(sop: SOP): SOP = {
      sop match {
        case r: Parallel => 
          r.copy(nodeID = UUID.randomUUID())
        case r: Sequence =>
          r.copy(nodeID = UUID.randomUUID())
        case r: OperationNode => {
          r.copy(nodeID = UUID.randomUUID())
        }
      }
    }

    def insertSop(root: SOP, targetId: UUID, sopId: UUID, direction: DropzoneDirection.Value): SOP = {
      //$.modState(s => s.copy())
      if(root.nodeID == targetId) {
        root match {
          case r: Parallel => {
            direction match {
              case DropzoneDirection.Left =>
                Parallel(
                  sop = cloneSop(findSop($.state.runNow().sop, sopId)):: r.sop
                )
              case DropzoneDirection.Right =>
                Parallel(
                   sop = r.sop :+ cloneSop(findSop($.state.runNow().sop, sopId))
                )
              case DropzoneDirection.Up =>
                Sequence(sop = List(cloneSop(findSop($.state.runNow().sop, sopId)), r))
              case DropzoneDirection.Down =>
                Sequence(sop = List(r, cloneSop(findSop($.state.runNow().sop, sopId))))
            }
          }
          case r: Sequence => {
            Sequence(nodeID = r.nodeID, sop = cloneSop(findSop($.state.runNow().sop, sopId)) :: r.sop)
          }
          case r: OperationNode => {
            direction match {
              case DropzoneDirection.Left =>
                Parallel(sop = List(cloneSop(findSop($.state.runNow().sop, sopId)), r))
              case DropzoneDirection.Right => 
                Parallel(sop = List(r, cloneSop(findSop($.state.runNow().sop, sopId))))
              case DropzoneDirection.Up =>
                Sequence(sop = List(cloneSop(findSop($.state.runNow().sop, sopId)), r))
              case DropzoneDirection.Down =>
                Sequence(sop = List(r, cloneSop(findSop($.state.runNow().sop, sopId))))
            }
          }
        }
      } else {
        root match {
          case r: Parallel =>
            Parallel(sop = r.sop.collect{case e => insertSop(e, targetId, sopId, direction)})
          case r: Sequence =>
            Sequence(sop = r.sop.collect{case e => insertSop(e, targetId, sopId, direction)})
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




