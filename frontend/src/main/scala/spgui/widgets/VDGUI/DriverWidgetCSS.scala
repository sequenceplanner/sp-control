package spgui.widgets.VDGUI

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

object DriverWidgetCSS extends StyleSheet.Inline {
  import dsl._

  val cardGroupCollapsed = style(
    flexGrow(1),
    display.block
   )

  val cardGroupExpanded = style(
    display.flex,
    flexGrow(1)
  )
  
  val rootDiv = style(
    height(100.%%),
    width(100.%%),
    display.flex,
    flexDirection.column
  )

  val cardPlaceholder = style(
    padding(8.px),
    display.inlineTable
  )

  val cardOuter = style(
    boxShadow:= "0px 1px 5px 0px rgba(0, 0, 0, 0.2), 0px 2px 2px 0px rgba(0, 0, 0, 0.14), 0px 3px 1px -2px rgba(0, 0, 0, 0.12)",
    display.flex,
    flexGrow(1),
    width(120.px),
    height(140.px),
    overflow.auto
  )

  val cardCollapsed = style(
    width(0.px),
    height(0.px),
    display.none
  )

  val cardExpanded = style(
    display.flex,
    width(100.%%)
  )

  val cardTitle = style(
    fontSize(20.px),
    textAlign.center,
    textOverflow:= ("ellipsis"),
    overflow.hidden
  )

  val unsetHeight = style(
    height.unset
  )

  this.addToDocument()
}
