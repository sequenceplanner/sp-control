package spgui.widgets.virtcom

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
    userSelect := "none",
  )

  this.addToDocument()
}
