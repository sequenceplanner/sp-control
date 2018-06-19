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
      state,
      ^.className := SopMakerCSS.sopComponent.htmlClass,
      ^.style := {
        var rect =  (js.Object()).asInstanceOf[Rect]
        rect.left = x
        rect.top = y
        rect.height = SopMakerWidget.opHeight
        rect.width = SopMakerWidget.opWidth
        rect
      },
      svg.svg(
        svg.width := SopMakerWidget.opWidth.toInt,
        svg.height:= SopMakerWidget.opHeight.toInt,
        svg.svg(
          svg.width := SopMakerWidget.opWidth.toInt,
          svg.height:= SopMakerWidget.opHeight.toInt,
          svg.x := 0,
          svg.y := 0,
          svg.rect(
            svg.x := 0,
            svg.y := 0,
            svg.width := SopMakerWidget.opWidth.toInt,
            svg.height:= SopMakerWidget.opHeight.toInt,
            svg.rx := 6, svg.ry := 6,
            svg.fill := "white",
            svg.stroke := "black",
            svg.strokeWidth := 1
          ),
          // Top horizontal line
          svg.rect(
            svg.x := 0,
            svg.y := SopMakerWidget.opHorizontalBarOffset.toInt,
            svg.width := SopMakerWidget.opWidth.toInt,
            svg.height:= 1,
            svg.rx := 0, svg.ry := 0,
            svg.fill := "black",
            svg.stroke := "black",
            svg.strokeWidth := 1
          ),
          // Bottom horizontal line
          svg.rect(
            svg.x := 0,
            svg.y :=  (SopMakerWidget.opHeight - SopMakerWidget.opHorizontalBarOffset).toInt,
            svg.width := SopMakerWidget.opWidth.toInt,
            svg.height:= 1,
            svg.rx := 0, svg.ry := 0,
            svg.fill := "black",
            svg.stroke := "black",
            svg.strokeWidth := 1
          ),
          // Left vertical line
          svg.rect(
            svg.x := SopMakerWidget.opVerticalBarOffset.toInt,
            svg.y := SopMakerWidget.opHorizontalBarOffset,
            svg.width := 1,
            svg.height:= SopMakerWidget.opHeight.toInt - 2*SopMakerWidget.opHorizontalBarOffset,
            svg.rx := 0, svg.ry := 0,
            svg.fill := "black",
            svg.stroke := "black",
            svg.strokeWidth := 1
          ),
          // Right vertical line
          svg.rect(
            svg.x := SopMakerWidget.opWidth - SopMakerWidget.opVerticalBarOffset.toInt,
            svg.y := SopMakerWidget.opHorizontalBarOffset,
            svg.width := 1,
            svg.height:= SopMakerWidget.opHeight.toInt - 2*SopMakerWidget.opHorizontalBarOffset,
            svg.rx := 0, svg.ry := 0,
            svg.fill := "black",
            svg.stroke := "black",
            svg.strokeWidth := 1
          ),
          svg.svg(
            svg.text(
              svg.x := "50%",
              svg.y := "50%",
              svg.textAnchor := "middle",
              svg.dy := ".3em",
              label
            )
          )
        )
      )
    )
  }

  def parallelBars(x: Float, y: Float, w:Float): TagMod =
    <.span(
      ^.className := SopMakerCSS.sopComponent.htmlClass,
      ^.style := {
        var rect =  (js.Object()).asInstanceOf[Rect]
        rect.left = x + SopMakerWidget.opWidth/2
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
        rect.left = x + SopMakerWidget.opWidth/2
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
