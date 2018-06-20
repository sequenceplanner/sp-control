package spgui.widgets.VDGUI

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

/** Define the css-classes for the DriverWidget with ScalaCSS */
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
    width(150.px),
    height(100.px),
    overflow.auto,
    position.relative
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


  val paddingCTS = 3.px
  val cardTitleSmall = style(
    fontSize(12.px),
    textOverflow:= ("ellipsis"),
    overflow.hidden,
    paddingTop(paddingCTS),
    paddingLeft(paddingCTS),
    paddingRight(paddingCTS),
    borderBottom(1.px),
    borderBottomStyle.solid,
    fontFamily:=!"'Arial'",
    cursor.pointer,
    &.hover(
      backgroundColor :=! "#eeeeee"
    )
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

  val resourceCard = style(
    width(100.%%),
    backgroundColor :=! "#cccccc"
  )

  val driverCard = style(
    width(100.%%),
    backgroundColor :=! "#cccccc"
  )

  val cardTitleExpanded = style(
    fontSize(20 px),
    fontWeight._500,
    borderBottom(1.px),
    borderBottomStyle.solid,
    borderBottomColor :=! "#888888",
    paddingTop(2.px),
    paddingLeft(10.px),
    paddingRight(10.px),
    fontFamily:=!"'Arial'",
    cursor.pointer,
    &.hover(
      backgroundColor :=! "#eeeeee"
    )
  )

  val stateList = style(
    fontFamily:=!"'Courier New', Courier, monospace",
    padding(12.px)
  )

  val cardSubtitle = style(
    fontSize(16 px),
    fontWeight._500,
    lineHeight(1.2),
    margin(10 px, 0 px, 10 px, 0 px),
    color.rgb(119, 119, 119)
  )

  val driverTypeSmall = style(
    marginLeft(4.px),
    fontSize(12 px),
    color :=! "#555555"
  )

  val driverType = style(
    marginLeft(12.px),
    color :=! "#555555"
  )

  val driverStates = style(
    fontFamily:=!"'Courier New', Courier, monospace",
    margin(12.px)
  )

  val driverStatus = style(
    marginLeft(12.px),
    color :=! "#555555"
  )

  val driverStatusSmall = style(
    marginLeft(4.px),
    fontSize(12 px),
    color :=! "#555555"
  )

  val driverOffline = style(
    color :=! "#ff0000",
    fontWeight :=! "bold"
  )

  val driverOnline = style(
    color :=! "#00ff00",
    fontWeight :=! "bold"
  )

  val driverUnresponsive = style(
    color :=! "#ffff00",
    fontWeight :=! "bold"
  )

  val buttonContainer = style(
    position.absolute,
    bottom(0 px),
    margin(4 px)
  )

  


  val table = style(
    tableLayout.auto,
    width(100 %%)
  )

  val input = style(
    width(100 %%),
    border.none,
    outline.none,
    background := "rgba(255,255,255,0.2)"
  )

  this.addToDocument()
}
