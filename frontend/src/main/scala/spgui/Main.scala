package spgui

import generators.{RNG, SopGeneration}
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
    Widgets.loadWidgets()
    new DashboardPresetsHandler()
    ModelCommunication.run()
    println(SopGeneration.showSop(SopGeneration.sop(RNG.Simple(12345))._1))
    SPMenu.addNavElem(ModelStatus())
    Layout().renderIntoDOM(document.getElementById("spgui-root"))
  }
}
