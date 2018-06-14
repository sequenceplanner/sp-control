package spgui.widgets.OPGUI

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

object OperationRunnerWidgetCSS extends StyleSheet.Inline {
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
    width(150.px),
    height(100.px),
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


  val marginCTS = 3.px
  val cardTitleSmall = style(
    fontSize(12.px),
    textOverflow:= ("ellipsis"),
    overflow.hidden,
    marginTop(marginCTS),
    marginLeft(marginCTS),
    marginRight(marginCTS),
    borderBottom(1.px),
    borderBottomStyle.solid,
    fontFamily:=!"'Arial'"
  )

  val unsetHeight = style(
    height.unset
  )

  val cardState = style(
    margin(0 px,0 px,0 px,25 px)
  )
  val cardGroupTitle = style(
    fontSize(20 px),
  )

  val card = style(
    width(100.%%),
    backgroundColor :=! "#cccccc"
  )

  this.addToDocument()
}
