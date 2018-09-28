package spgui.widgets.VDGUI

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._
import CSSHelpers.Child.child


/** Define the css-classes for the DriverWidget with ScalaCSS */
object TableCSSRedesign extends StyleSheet.Inline {
  import dsl._

  private val space = 10 px
  private val headerColor = c"#7B8383"
  private val valueColor = c"#606879"
  private val borderColor = c"#DEE8E8"
  private val headerBgColor = c"#F2F6F6"
  private val cellHoverColor = c"#F1F4FA"
  private val bgColor = c"#FFFFFC"

  private val font = "'Roboto', sans-serif"

  val none = style()

  val table = style(
    width :=! "calc(100% - 15px)",
    tableLayout.auto,
    fontFamily :=! font
  )

  val cell = style(
    maxWidth(200 px),
    padding(space),
    margin.`0`,
    fontSize(11 pt),
    minWidth(80 px),
    whiteSpace.nowrap,
    overflow.hidden,
    textOverflow := "ellipsis",
    borderLeft :=! s"2px solid ${borderColor.value}",
    borderBottom :=! s"2px solid ${borderColor.value}"
  )

  val headerCell = style(
    cell,
    padding(8 px),
    backgroundColor(headerBgColor),
    color(headerColor),
    borderTop :=! s"2px solid ${borderColor.value}"
  )

  val dataCell = style(
    cell,
    color(valueColor),
    backgroundColor(bgColor),
    textOverflow := "ellipsis",
    whiteSpace.nowrap,
    overflow.hidden
  )

  val hoveredCell = style(
    backgroundColor(cellHoverColor)
  )

  val row = style(
    child.td(
      &.lastChild(
        borderRight :=! s"2px solid ${borderColor.value}"
      )
    ),
    child.th(
      &.lastChild(
        borderRight :=! s"2px solid ${borderColor.value}"
      )
    )
  )

  this.addToDocument()
}
