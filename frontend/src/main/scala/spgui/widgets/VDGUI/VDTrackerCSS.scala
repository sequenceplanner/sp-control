package spgui.widgets.VDGUI

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

object VDTrackerCSS extends StyleSheet.Inline  {
  import dsl._
  import spgui.CSSConfig.space

  val tableDescriptionText = style(
    marginLeft(space)
  )

  val icon = style(
    fontSize(14 pt),
    paddingRight(0.5 em)
  )

  val smallColumn = style(
    maxWidth(space * 3)
  )

  val valueColumn = style(
    minWidth(225 px).important,

    unsafeChild(".data")(
      fontFamily :=! "'Source Code Pro', monospace"
    )
  )

  val details = style(
    outline.none
  )

  this.addToDocument()
}
