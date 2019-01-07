package spgui.widgets

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import spgui.SPWidget

object TestComponents {
  private class Backend($: BackendScope[Unit, Unit]) {
    def render() = {
      <.div()
    }
  }

  private val component = ScalaComponent.builder[Unit]("VDTracker")
    .renderBackend[Backend]
    .build

  def apply() = SPWidget(spwb => component())
}
