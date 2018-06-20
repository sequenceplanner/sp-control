package spgui.widgets.model

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._


object ModelStatusCSS extends StyleSheet.Inline {
  import dsl._

  val container = style(
    paddingTop(0.5 em),
    display.flex,
    alignItems.center
  )

  val innerContainer = style(
    display.flex,
    flexDirection.column
  )

  val icon = style(
    fontSize(14 pt),
    paddingRight(0.5 em)
  )

  val text = style(
    margin.`0`,
    color :=! "#454545"
  )

  val smallText = style(
    text,
    fontSize(7 pt),
    textTransform.uppercase
  )

  val normalText = style(
    text,
    fontSize(10 pt),
    textOverflow := "ellipsis",
    overflow.hidden,
    whiteSpace.nowrap,
    maxWidth(200 px)
  )

  this.addToDocument()
}
