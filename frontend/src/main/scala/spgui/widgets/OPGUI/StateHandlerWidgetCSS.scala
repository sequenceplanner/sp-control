package spgui.widgets.OPGUI

import scalacss.DevDefaults._
import scalacss.ScalaCssReact._
import spgui.theming.Theming

object StateHandlerWidgetCSS extends Theming.SPStyleSheet {
  import dsl._

  val textBoxDiv = style(
    float.right
  )

  this.addToDocument()
}
