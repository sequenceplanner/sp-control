package spgui.widgets.virtcom

import org.scalajs.dom.raw.CSSKeyframesRule
import spgui.components.SPWidgetElementsCSS.{_rgb, theme}

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._



object Style extends StyleSheet.Inline {
  import dsl._

  val scrollDropDown = style(
    height.auto,
    maxHeight(200 px),
    overflowY.auto,
    overflowX.hidden
  )


  val schSelect = style(
    width.maxContent,
  )

  val Table = style(
    tableLayout.fixed,
    width(100 %%)
  )

  val inputs = style(
 display.flex
  )
  val buttons = style(
    padding(4.px),
    display.inlineBlock,
    margin(2.px),
    borderRadius(0.px),
    borderColor(_rgb(theme.value.widgetButtonBorderColor)),
    backgroundColor(_rgb(theme.value.widgetButtonBackgroundColor)),
    color(_rgb(theme.value.defaultTextColor)).important,

  )

  val dropWidth = style(
    display.inlineBlock,
    overflowX.auto
  )

  val lngCaseHide =style(
    position.absolute,
    visibility.hidden,
    height.auto,
    width.auto,
    whiteSpace.nowrap
  )

  val collapsible = style(
    cursor.pointer,
    outline.none
    //userSelect := "none",
  )



  val rotate = keyframes(
    (0%%) -> keyframe(transform := "rotate(0deg)"),
    (100%%) -> keyframe(transform := "rotate(360deg)")
  )

  val loader = mixin(

    position.absolute,
    top(50 %%), // center
    left(50 %%),

    borderTop.attr    := "DarkGray  8px solid", // color size sides of rectangle
    borderLeft.attr   := "LightGray 8px solid",
    borderRight.attr  := "LightGray 8px solid",
    borderBottom.attr := "DarkGray  8px solid",

    borderRadius(50 %%), // circle

    animationName(rotate),
    animationIterationCount.infinite,
    animationTimingFunction.linear // continuous
  )

  val spinLoader = style(
    loader,
    height(100.px),
    width(100.px),
    animationDuration.attr := "3s",
    marginTop(-50.px), // adjust to center
    marginLeft(-50.px),
    display.none
  )


  this.addToDocument()
}
