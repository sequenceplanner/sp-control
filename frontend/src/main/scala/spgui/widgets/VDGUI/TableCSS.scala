package spgui.widgets.VDGUI

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

/** Define the css-classes for the DriverWidget with ScalaCSS */
object TableCSS extends StyleSheet.Inline {
  import dsl._

  private val space = 1 em
  private val headerColor = c"#F57C00"
  private val valueColor = c"#000"

  private val font = "'Lato', sans-serif"

  val none = style()

  val data = style(
    paddingTop(space / 2),
    paddingBottom(space / 2),
    margin.`0`,
    opacity(0.85),
    fontSize(13.px),
    color(headerColor)
  )

  val value = style(
    data,
    display.inlineBlock,
    textOverflow := "ellipsis",
    whiteSpace.nowrap,
    overflow.hidden,
    color(valueColor)
  )

  val flexParent = style(
    display.flex,
    alignItems.center,
    padding(space),
    fontFamily :=! font,
  )

  val flexChild = style(
    display.flex,
    flexDirection.column,
    padding(space * 2)
  )

  val shortAndFixed = style(
    whiteSpace.nowrap
  )

  val shortFixedDiv = style(
    width(30 px),
    height(30 px),
    borderRadius(10 px),
    backgroundColor.lightgreen,
    display.inlineBlock
  )

  val truncated = style(
    minWidth(0 px),
    whiteSpace.nowrap,
    overflow.hidden,
    textOverflow := "ellipsis"
  )

  val truncThing = style(

  )

  this.addToDocument()
}
