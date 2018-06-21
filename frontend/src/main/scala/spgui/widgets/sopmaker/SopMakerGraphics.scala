package spgui.widgets.sopmaker

import scala.scalajs.js
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.all.svg

import spgui.components.SPWidgetElementsCSS
import spgui.components.SPWidgetElements
import java.util.UUID

import spgui.dragging._

object SopMakerGraphics {

  trait Rect extends js.Object {
    var left: Float = js.native
    var top: Float = js.native
    var width: Float = js.native
    var height: Float = js.native
  }

  def op(label: String, x: Int, y: Int, state: String = "NoneState") = {
    <.span(
      ^.className := SopMakerCSS.sopComponent.htmlClass,
      ^.style := {
        var rect =  (js.Object()).asInstanceOf[Rect]
        rect.left = x
        rect.top = y
        rect.height = SopVisualiser.opHeight
        rect.width = SopVisualiser.opWidth
        rect
      },
      <.div(
        ^.className := SopMakerCSS.opInner.htmlClass,
        <.div(
          ^.className := SopMakerCSS.preCondition.htmlClass
        ),
        <.div(
          ^.className := SopMakerCSS.opNameOuter.htmlClass,
          {
            state match {
              case "\"i\"" => ^.className := SopMakerCSS.opStateInit.htmlClass
              case "\"e\"" => ^.className := SopMakerCSS.opStateExec.htmlClass
              case "\"f\"" => ^.className := SopMakerCSS.opStateFini.htmlClass
              case _ => ^.className := SopMakerCSS.opStateNone.htmlClass
            }
          },
          <.div(
            ^.className := SopMakerCSS.opName.htmlClass,
            label
          )
        ),
        <.div(
          ^.className := SopMakerCSS.postCondition.htmlClass
        )
      )
    )
  }

  def parallelBars(x: Float, y: Float, w:Float): TagMod =
    <.span(
      ^.className := SopMakerCSS.sopComponent.htmlClass,
      ^.style := {
        var rect =  (js.Object()).asInstanceOf[Rect]
        rect.left = x + SopVisualiser.opWidth/2
        rect.top = y
        rect.height = 12
        rect.width = w
        rect
      },
      svg.svg(
        svg.width := "100%",
        svg.svg(
          svg.width := w.toInt,
          svg.height := 12,
          svg.rect(
            svg.x := 0,
            svg.y := 0,
            svg.width:=w.toInt,
            svg.height:=4,
            svg.fill := "black",
          ),
          svg.rect(
            svg.x := 0,
            svg.y :=  8,
            svg.width:=w.toInt,
            svg.height:=4,
            svg.fill := "black",
          )
        )
      )
    )

  def sopConnectionLine(x: Float, y: Float, h:Float): TagMod =
    <.span(
      ^.className := SopMakerCSS.sopComponent.htmlClass,
      ^.style := {
        var rect =  (js.Object()).asInstanceOf[Rect]
        rect.left = x + SopVisualiser.opWidth/2
        rect.top = y
        rect.height = h
        rect.width = 4
        rect
      },
      svg.svg(
        ^.className := SopMakerCSS.sopComponentSVG.htmlClass,
        svg.width := "100%",
        svg.svg(
          svg.width := 4,
          svg.height := h.toInt,
          svg.rect(
            svg.x := 0,
            svg.y := 0,
            svg.width:=4,
            svg.height:=h.toInt,
            svg.fill := "black",
          )
        )
      )
    )

  def menuOp(label: String, id: UUID): TagMod =
    <.span(
      //SPWidgetElements.draggable(label, null, "sop", (d:DropData) => println("static op drop")),
      ^.className := SopMakerCSS.menuOp.htmlClass,
      ^.className := SPWidgetElementsCSS.defaultMargin.htmlClass,
      svg.svg(
        ^.className := SopMakerCSS.menuOpInner.htmlClass,
        svg.viewBox := "0 0 100 60",
        svg.width := "48px",
        svg.height := "32px",
        svg.rect(
          svg.width := "100%",
          svg.height := "100%",
          svg.rx := 6, svg.ry := 6,
          svg.fill := "white",
          svg.stroke := "black",
          svg.strokeWidth := 1,
        ),
        svg.svg(
          svg.text(
            ^.className := SopMakerCSS.menuOpText.htmlClass,
            svg.x := "50%",
            svg.y := "50%",
            svg.textAnchor := "middle",
            svg.dy := ".3em",
            label
          )
        )
      )
    )
}
