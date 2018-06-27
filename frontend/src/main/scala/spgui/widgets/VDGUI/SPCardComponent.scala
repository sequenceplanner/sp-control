package spgui.widgets.VDGUI
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import play.api.libs.json.{JsString, JsValue}
import scalacss.internal.StyleA
import sp.domain._
import spgui.communication._
import sp.devicehandler.VD.Driver
import sp.devicehandler.APIDeviceDriver
import spgui.circuits.main.handlers.DriverHandler.{DriverId, DriverStatus}
import spgui.components.SPWidgetElements

import scala.util.Try

/** CardComponent for the DriverCard and ResourceCard */
object SPCardComponent {
  import spgui.widgets.VDGUI.{DriverWidgetCSS => css}

  case class State(expandedId: Option[ID] = None)
  case class Props(cards: List[CardInfo])

  private def expandedId = Lens[State, Option[ID]](_.expandedId)(a => _.copy(expandedId = a))

  trait CardInfo {
    val cardId: ID
  }

  type DriverName = String
  case class DriverCard(cardId: ID, name: String, status: String, `type`: String, setup : SPAttributes, state: Map[String, SPValue]) extends CardInfo
  case class ResourceCard(cardId: ID, name: String, driverStatuses: List[(DriverName, DriverStatus)], state: List[(String, SPValue)]) extends CardInfo

  class Backend($: BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      val expandedCard = state.expandedId.flatMap(id => props.cards.find(_.cardId == id))

      val content = expandedCard.fold(overviewCards(props.cards)) {
        case card: DriverCard => driverDetailCard(card)
        case card: ResourceCard => resourceDetailCard(card)
      }

      <.div(css.container, content)
    }

    def overviewCards(cards: List[CardInfo]): TagMod = {
      cards.map {
        case card: DriverCard => driverOverviewCard(card)
        case card: ResourceCard => resourceOverviewCard(card)
      }.toTagMod
    }

    def driverOverviewCard(card: DriverCard): TagMod = {
      <.div(
        css.overviewCard,
        <.div(css.cardTitleSmall, ^.onClick --> showDetailedView(card.cardId), card.name),
        <.div(css.driverName, <.div("Type: " + card.`type`)),
        <.div(css.driverStatus, <.span("Status: "), renderDriverStatus(card.status))
      )
    }

    def driverDetailCard(card: DriverCard): TagMod = {
      val cardStates = card.state.map { case (key, value) => <.tr(
        <.td(key),
        <.td(value.toString()),
        <.td(
          <.input(
            css.input,
            ^.placeholder := "Change value...",
            ^.onKeyPress ==> updateDriverState(card, key)
          )
        ))
      }.toTagMod

      <.div(
        css.resourceDetailCard,
        <.div(css.cardTitleExpanded, ^.onClick --> showDetailedView(card.cardId), card.name),
        <.div(css.driverName, <.div("Type: " + card.`type`)),
        <.div(css.driverStatus, <.span("Status: "), renderDriverStatus(card.status)),
        <.table(css.stateTable.table, <.tbody(cardStates)),
        <.span(
          SPWidgetElements.buttonGroup(Seq(
            SPWidgetElements.button("Terminate Driver", onTerminateDriver(card.cardId)),
            SPWidgetElements.button("Start Driver", onStartDriver(card))
          ))
        )
      )
    }

    def resourceOverviewCard(card: ResourceCard): TagMod = {
      val drivers = card.driverStatuses.map((renderDriver _).tupled).toTagMod
      <.div(
        css.overviewCard,
        ^.onClick --> showDetailedView(card.cardId),
        <.p(css.cardTitleSmall, card.name),
        drivers
      )
    }

    private val jsValueToString: PartialFunction[JsValue, String] = {
      case JsString(v) => v
      case x => x.toString()
    }

    def resourceDetailCard(card: ResourceCard): TagMod = {
      val drivers = card.driverStatuses.map((renderDriver _).tupled).toTagMod
      val header = ("Name", "Value")
      val data = card.state.map { case (k, v) => (k, jsValueToString(v)) }

      <.div(
        css.resourceDetailCard,
        ^.onClick --> closeDetailView,
        <.div(css.cardTitleExpanded, card.name),
        <.div(css.spacing, drivers),
        Table(header, data, closeDetailView)
        // <.div(css.stateTable.table, states)
      )
    }

    private val showDetailedView: ID => Callback = cardId => $.modState(expandedId.set(Some(cardId)))
    private val closeDetailView: Callback = $.modState(expandedId.set(None))

    private def renderDriverStatus(status: String): TagMod = {
      val driverCss: TagMod = status match {
        case DriverStatus.Online => css.driver.online
        case DriverStatus.Offline => css.driver.offline
        case DriverStatus.Unresponsive => css.driver.unresponsive
        case DriverStatus.Terminated => css.driver.terminated
        case _ => EmptyVdom
      }

      <.span(driverCss, status)
    }

    private def renderDriver(name: String, status: String): TagMod = {
      <.div(css.driverStatus, <.span(css.driverName, name), renderDriverStatus(status))
    }

    private def onStartDriver(card: DriverCard): Callback = Callback {
      DriverCommunication.postRequest(APIDeviceDriver.SetUpDeviceDriver(Driver(card.name, card.cardId, card.`type`, card.setup)))
    }

    private def onTerminateDriver(driverId: DriverId): Callback = Callback {
      DriverCommunication.postRequest(APIDeviceDriver.TerminateDriver(driverId))
    }

    def createCorrectTypeOfSPValue(newValue: String) : SPValue =  { // Convert the incoming string to an SPvalue of the same type as the previous state value
        Try(SPValue(newValue.toInt))
          .orElse(Try(SPValue(newValue.toBoolean)))
          .getOrElse(SPValue(newValue))
    }

    def updateDriverState(card: DriverCard, s1 : String)(e: ReactKeyboardEventFromInput): Callback = Callback {
      if(e.key == "Enter") {
        val newState = card.state + (s1 -> createCorrectTypeOfSPValue(e.target.value))
        DriverCommunication.postRequest(APIDeviceDriver.DriverCommand(card.cardId, newState))
      }
    }
  }


  private val component = ScalaComponent.builder[Props]("CardGrid")
    .initialState(State())
    .renderBackend[Backend]
    .build

  implicit def toHtml(a: StyleA): TagMod = ^.className := a.htmlClass


  def apply(cards: List[CardInfo]) = component(Props(cards))
}
