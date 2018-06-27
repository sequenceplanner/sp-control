package spgui.widgets.VDGUI

import japgolly.scalajs.react.vdom.html_<^._
import scalacss.internal.StyleA

object CSSHelpers {
  implicit def toHtml(a: StyleA): TagMod = ^.className := a.htmlClass
}
