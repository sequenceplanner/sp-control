package spgui.widgets.itemexplorerincontrol

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._

object Style extends StyleSheet.Inline {
  import dsl._

  val outerDiv = style(
    display.flex,
    flexDirection.column,
    height(100 %%),
    minWidth(100 %%),
    display.inlineFlex
  )

  val optionPane = style(
    position.relative,
    display.flex,
    flexDirection.row
  )

  val newItems = style(
    backgroundColor.white,
    unsafeChild("ul")(
      listStyleType := none,
      unsafeChild("li")(
        float.left
      )
    )
  )

  val structsView = style(
    backgroundColor.white,
    height(100 %%)
  )

  val ul = style(
    listStyleType := none, // removes the bullet
    whiteSpace.nowrap // make li:s take up as much horizontal space as they wish
  )

  val nodeOuter = style(
    display.flex
  )

  this.addToDocument()
}
