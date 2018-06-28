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
  val column = style(
    display.flex,
    flexDirection.column,
    padding(space),
    width :=! "calc(50% - 2em)"
  )

  val none = style()

  val data = style(
    paddingTop(space / 2),
    paddingBottom(space / 2),
    margin.`0`,
    color(black),
    opacity(0.75),
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

  val centerContent = style(
    display.flex,
    justifyContent.center
  )

  val body = style(
    display.flex,
    width(100 %%),
    fontFamily :=! font,
    marginBottom(space)
  )

  this.addToDocument()
}
