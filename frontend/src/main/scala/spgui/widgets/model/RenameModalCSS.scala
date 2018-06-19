package spgui.widgets.model

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._


object RenameModalCSS extends StyleSheet.Inline {
  import dsl._

  val container = style(
    paddingTop(0.5.em),
    display.flex,
    flexDirection.column,
    alignItems.center
  )

  val innerContainer = style(
    display.flex,
    alignSelf.stretch,
    justifyContent.spaceBetween
  )

  val button = style(
    margin(0.5.em)
  )

  this.addToDocument()
}
