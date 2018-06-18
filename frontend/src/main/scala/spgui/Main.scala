package spgui

import org.scalajs.dom.document
import spgui.menu.SPMenu
import japgolly.scalajs.react.vdom.html_<^._
import spgui.widgets.model.ModelStatus

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object Main extends App {
  @JSExportTopLevel("spgui.Main")
  protected def getInstance(): this.type = this

  @JSExport
  def main(): Unit = {
    LoadingWidgets.loadWidgets
    ModelCommunication.run()
    SPMenu.addNavElem(ModelStatus())
    Layout().renderIntoDOM(document.getElementById("spgui-root"))
  }
}
