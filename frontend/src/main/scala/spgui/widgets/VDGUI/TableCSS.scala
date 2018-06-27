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
    width :=! "calc(50% - 2em)",

    &.lastChild(
      textAlign.center,
      fontWeight.bold
    )
  )

  val data = style(
    padding(space / 2),
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

  val body = style(
    display.flex,
    width(100 %%),
    fontFamily :=! font,
    padding(space),
    backgroundColor(white),
    marginBottom(space)
  )

  this.addToDocument()
}
