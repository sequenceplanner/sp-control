package spgui.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import spgui.components.Icon.Icon

object Button {
  def SPButton(text: String, onClick: Callback, icon: Icon = null) =
    <.button(
      ^.onClick --> onClick,
      ^.className := ButtonCSS.spButtonStyle.htmlClass,
      if(icon == null) text
      else
        <.span(<.span(^.className := ButtonCSS.textBeforeIcon.htmlClass, text), icon)
    )

  def SPDropdown(text: String, onClick: Callback, icon: Icon = null, subItems: Seq[TagOf[html.Button]] = Seq(), css: String = "") =
    <.button(
      ^.onClick --> onClick,
      ^.className := ButtonCSS.spDropdownButtonStyle.htmlClass,
      ^.className := css,
      <.span(^.className := ButtonCSS.textBeforeIcon.htmlClass, text),
      icon,
      <.ul(
        ^.className := ButtonCSS.unsortedList.htmlClass, // add css for a list
        subItems.map(node =>
          <.li(
            ^.className := ButtonCSS.listItem.htmlClass, // add css foreach list item
            node
          )
        ).toTagMod
      ).when(subItems.nonEmpty)
    )

  def SPDropdownItem(text: String, onClick: Callback) =
    <.button(
      ^.onClick --> onClick,
      ^.className := ButtonCSS.spDropdownItem.htmlClass,
      text
    )

}
