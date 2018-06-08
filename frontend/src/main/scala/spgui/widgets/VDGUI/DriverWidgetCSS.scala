package spgui.widgets.VDGUI

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

object DriverWidgetCSS extends StyleSheet.Inline {
  import dsl._

  val cardGroup = style(
    height(100.%%),
    width(100.%%)
  )

  val rootDiv = style(
    height(100.%%),
    width(100.%%)
  )

  val cardOuter = style(
    width(120.px),
    height(140.px),
    display.inlineBlock,
    boxShadow:= "0px 1px 5px 0px rgba(0, 0, 0, 0.2), 0px 2px 2px 0px rgba(0, 0, 0, 0.14), 0px 3px 1px -2px rgba(0, 0, 0, 0.12)",
    marginLeft(10.px),
    marginTop(5.px)
  )

  val cardCollapsed = style(
    width(0.px),
    height(0.px),
    display.none
  )

  val cardExpanded = style(
    height(100.%%),
    width(100.%%)
  )

  val cardTitle = style(
    fontSize(20.px),
    textAlign.center
  )

  this.addToDocument()
}
