package spgui.widgets.VDGUI

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import spgui.SimpleSet
import spgui.circuits.main.MainCircuit
import spgui.circuits.main.handlers.DriverHandler.DriverId
import spgui.circuits.main.handlers.DriverInfo
import spgui.widgets.VDGUI.SPCardComponent.CardInfo


/** Widget for visualising the drivers status */
object DriverWidget {
  case class Props(proxy: ModelProxy[SimpleSet[DriverId, DriverInfo]]) {
    val drivers: SimpleSet[DriverId, DriverInfo] = proxy.value
  }

  val cards: Props => List[CardInfo] = _.drivers.toList.map { driver =>
    SPCardComponent.DriverCard(
      cardId = driver.id,
      name = driver.name,
      status = driver.status,
      `type` = driver.driverType,
      setup = driver.setup,
      state = driver.state
    )
  }

  val cardComponent: List[CardInfo] => VdomElement = list => SPCardComponent(list).vdomElement

  private val driverWidgetComponent = ScalaComponent.builder[Props]("DriverWidget")
    .render_P(cardComponent compose cards)
    .build

  private val connectComponent = MainCircuit.connectComponent(_.drivers.drivers)
  def apply() = spgui.SPWidget(_ => connectComponent { proxy => driverWidgetComponent(Props(proxy)) })
}
