package spgui.widgets.VDGUI

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain.{ID, SPAttributes, SPValue}
import spgui.SimpleSet
import spgui.circuits.main.MainCircuit
import spgui.circuits.main.handlers.DriverHandler.DriverId
import spgui.circuits.main.handlers.DriverInfo
import spgui.widgets.VDGUI.cards.DriverView

/** Widget for visualising the drivers status */
object DriverWidget {
  case class DriverCard(cardId: ID, name: String, status: String, `type`: String, setup : SPAttributes, state: Map[String, SPValue])

  case class Props(proxy: ModelProxy[SimpleSet[DriverId, DriverInfo]]) {
    val drivers: SimpleSet[DriverId, DriverInfo] = proxy.value
  }

  implicit val simpleSetReusability: Reusability[DriverInfo] = Reusability.by_==
  implicit val propsReusability: Reusability[Props] = Reusability.by(_.drivers.toList)

  private class Backend($: BackendScope[Props, Option[ID]]) {

    def render(props: Props, expandedId: Option[ID]): VdomElement = {
      val expandedCard = expandedId.flatMap(id => props.drivers.find(_.id == id)).map(cardFromInfo)

      expandedCard match {
        case Some(card) => DriverView.Detail(card, onClick = $.setState(None))
        case None => DriverView.Overview(props.drivers.map(cardFromInfo), onCardClick = cardId => $.setState(Some(cardId)))
      }
    }
  }

  private def cardFromInfo(driverInfo: DriverInfo): DriverCard = {
    DriverCard(
      cardId = driverInfo.id,
      name = driverInfo.name,
      status = driverInfo.status,
      `type` = driverInfo.driverType,
      setup = driverInfo.setup,
      state = driverInfo.state
    )
  }

  private val driverWidgetComponent = ScalaComponent.builder[Props]("DriverWidget")
    .initialState(Option.empty[ID])
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build

  private val connectComponent = MainCircuit.connectComponent(_.drivers.drivers)
  def apply() = spgui.SPWidget(_ => connectComponent { proxy => driverWidgetComponent(Props(proxy)) })
}
