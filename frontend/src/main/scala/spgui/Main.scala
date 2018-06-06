package spgui

import org.scalajs.dom.document
import spgui.dashboard.DashboardPresetsConnector

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object Main extends App {
  @JSExportTopLevel("spgui.Main")
  protected def getInstance(): this.type = this

  @JSExport
  def main(): Unit = {
    LoadingWidgets.loadWidgets
    DashboardPresetsConnector.addPresetMenu()
    Layout().renderIntoDOM(document.getElementById("spgui-root"))
  }
}
