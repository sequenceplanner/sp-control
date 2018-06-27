package spgui.widgets.VDGUI.cards

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

/** Define the css-classes for the DriverWidget with ScalaCSS */
object CardViewCSS extends StyleSheet.Inline {
  import dsl._

  private val space = 15 px
  private val bgColor = white
  private val primaryColor = c"#F57C00"
  private val secondaryColor = c"#000"
  private val titleFont = "'Roboto', sans-serif"
  private val tableFont = "'Lato', sans-serif"

  val center = style(
    textAlign.center,
    display.flex,
    justifyContent.center,
    fontWeight.bold
  )

  val spacing = style(
    padding(space)
  )


  val cardHover = style(
    transition := "box-shadow 0.3s cubic-bezier(.25, .8, .25, 1)",

    &.hover(
      boxShadow := "0 14px 28px rgba(0, 0, 0, 0.25), 0 10px 10px rgba(0, 0, 0, 0.22)",
    )
  )

  val card = style(
    margin(space / 2),
    padding(space),
    display.inlineFlex,
    flexDirection.column,
    boxShadow := "0 1px 3px rgba(0, 0, 0, 0.12), 0 1px 2px rgba(0, 0, 0, 0.24)",
    backgroundColor(bgColor),
    cursor.pointer
  )

  val overviewCard = style(
    card,
    cardHover,
  )

  val resourceDetailCard = style(
    card,
    width :=! "calc(100% - 15px)"
  )

  val resetP = style(
    margin.`0`,
    display.inline
  )

  val cardTitleSmall = style(
    resetP,
    paddingBottom(space),
    paddingRight(space),
    fontSize(13.px),
    color(primaryColor),
    fontFamily :=! titleFont,
    textOverflow:= "ellipsis",
    fontWeight.bold,
    overflow.hidden,
    cursor.pointer,
  )

  val cardTitleExpanded = style(
    cardTitleSmall,
    fontSize(18.px),
  )

  val driverStatus = style(
    fontFamily :=! tableFont,
    display.flex,
    justifyContent.spaceBetween,
    maxWidth(180 px),
  )

  val driverName = style(
    width(80 px),
    paddingRight(space / 2),
    textOverflow := "ellipsis",
    overflow.hidden
  )

  val driverInfo = style(
    display.flex,
    fontFamily :=! tableFont
  )

  val driverInfoColumn = style(
    display.flex,
    flexDirection.column,
    paddingRight(1 em),
    opacity(0.9)
  )

  val driverInfoColumnRight = style(
    driverInfoColumn,
    opacity(1),
    color(c"#444"),
    fontWeight.bold,
    alignItems.flexEnd,
    textAlign.end
  )

  object stateTable {
    val table = style(
      display.flex,
      flexDirection.column,
      fontFamily :=! tableFont,
      padding(space),
      backgroundColor(bgColor),
      marginBottom(space)
    )

    val row = style(
      paddingBottom(space),
      &.lastChild {
        paddingBottom.`0`
      }
    )

    private val rowData = style(
      display.inlineBlock,
      margin.`0`,
      maxWidth(80 px),
      minWidth(80 px),
      color(black),
      opacity(0.75)
    )

    val rowName = style(
      rowData
    )

    val rowValue = style(
      rowData,
      textAlign.center,
      fontWeight.bold
    )
  }

  object driver {
    val value = style(
      fontWeight.bold
    )

    val online = style(value, color(c"#66BB6A"))
    val offline = style(value, color(c"#b71c1c"))
    val unresponsive = style(value, color(c"#8D6E63"))
    val terminated = style(value, color(c"#90A4AE"))
  }

  val input = style(
    width(100 %%),
    border.none,
    outline.none,
    background := "rgba(255,255,255,0.2)"
  )

  initInnerObjects(stateTable.table, driver.value)

  this.addToDocument()
}
