package spgui.widgets.itemexplorerincontrol

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

object Style extends StyleSheet.Inline {
  import dsl._

  val optionPane = style(
    position.relative,
    display.flex,
    flexDirection.row
  )

}
