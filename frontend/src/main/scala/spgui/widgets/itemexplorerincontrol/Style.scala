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
    height(100 %%),
    overflow.auto,
    fontFamily :=! "monospace",
    paddingLeft(15 px)
  )

  val ul = style(
    listStyleType := none, // removes the bullet
    whiteSpace.nowrap, // make li:s take up as much horizontal space as they wish
    paddingLeft(15 px)
  )

  val nodeOuter = style(
    display.flex
  )

  val itemOuter = style(

  )

  val itemIcon = style(
    marginRight(3 px)
  )

  val itemName = style(

  )

  val expansionToggler = style(
    left(-15 px),
    position.absolute
  )

  this.addToDocument()
}
