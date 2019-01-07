package spgui.widgets.itemexplorerincontrol

import diode.react.ModelProxy
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import sp.domain.ID
import spgui.circuits.availablemodelscircuit.{AvailableModels, AvailableModelsCircuit}
import spgui.components.Button._
import spgui.components.Icon

object ModelChoiceDropdown {
  case class Props(proxy: ModelProxy[AvailableModels], cb: ID => Callback)

  val avmcConnection = AvailableModelsCircuit.connect(x => x)

  val component = ScalaComponent.builder[Props]("ModelChoiceDropdown")
    .render_P { p =>
      val contents = p.proxy().models.toList.map(kv => SPDropdownItem(kv._2, p.cb(kv._1)))
      SPDropdown("Choose Model", Callback.empty, Icon.arrowDown, contents)
    }
    .build

  def apply(cb: ID => Callback) = avmcConnection(proxy => component(Props(proxy, cb)))
}
