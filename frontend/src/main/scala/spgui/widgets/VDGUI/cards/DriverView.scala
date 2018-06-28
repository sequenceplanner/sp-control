package spgui.widgets.VDGUI.cards

import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ReactKeyboardEventFromInput, ScalaComponent}
import play.api.libs.json.{JsObject, JsValue}
import sp.devicehandler.APIDeviceDriver
import sp.devicehandler.VD.Driver
import sp.domain.{ID, SPValue}
import spgui.circuits.main.handlers.DriverHandler.DriverId
import spgui.communication.DriverCommunication
import spgui.components.{SPWidgetElements, SPWidgetElementsCSS}
import spgui.widgets.VDGUI.DriverWidget.DriverCard
import spgui.widgets.VDGUI.{ResourceWidget, Table}
import spgui.widgets.VDGUI.Table.ColumnData
import spgui.widgets.VDGUI.cards.{CardViewCSS => css}
import spgui.widgets.VDGUI.CSSHelpers.toHtml

import scala.util.Try

object DriverView {
  implicit val jsValueReusability: Reusability[JsValue] = Reusability.by(_.toString())
  implicit val jsObjectReusability: Reusability[JsObject] = Reusability.by_==
  implicit val mapReusability: Reusability[Map[String, SPValue]] = Reusability.by_==

  object Overview {
    case class Props(cards: Iterable[DriverCard], onCardClick: ID => Callback)

    implicit val cardReusability: Reusability[DriverCard] = Reusability.by((card: DriverCard) => card.status)

    implicit val driversReusability: Reusability[Iterable[DriverCard]] = Reusability.byIterator[Iterable, DriverCard]
    implicit val propsReusability: Reusability[Props] = Reusability.by(_.cards)

    def overviewCard(props: Props, card: DriverCard): TagMod = {
      <.div(
        ^.onClick --> props.onCardClick(card.cardId),
        css.overviewCard,
        <.div(css.cardTitleSmall, card.name),
        <.div(
          css.driverInfo,
          <.div(css.driverInfoColumn, <.p(css.resetP, "Type"), <.p(css.resetP, "Status")),
          <.div(css.driverInfoColumnRight, <.p(css.resetP, card.`type`), ResourceWidget.driverStatus(card.status))
        )
      )
    }

    def render(props: Props): VdomElement = {
      val cards = props.cards.map(card => overviewCard(props, card)).toTagMod

      <.div(cards).render
    }

    private val component = ScalaComponent.builder[Props]("ResourceOverviewView")
      .render_P(render)
      .configure(Reusability.shouldComponentUpdate)
      .build

    def apply(cards: Iterable[DriverCard], onCardClick: ID => Callback): VdomElement = component(Props(cards, onCardClick))
  }

  object Detail {

    case class Props(card: DriverCard, onClick: Callback)

    implicit val hmReuse: Reusability[(String, JsValue)] = Reusability.tuple2
    implicit val stateReusability: Reusability[List[(String, JsValue)]] = Reusability.byIterator
    implicit val cardReusability: Reusability[DriverCard] = Reusability.derive
    implicit val reusability: Reusability[Props] = Reusability.by((props: Props) => props.card)

    private def render(props: Props): VdomElement = {
      val card = props.card
      val tableData = card.state.map { case (key, value) =>
        val input = <.input(
          css.input,
          ^.onClick ==> (e => e.stopPropagationCB),
          ^.placeholder := "Change value...",
          ^.onKeyPress ==> updateDriverState(card, key)
        ).render

        (key, value, input)
      }

      val columnData = Vector(
        ColumnData("Name"),
        ColumnData("Value", css.center),
        ColumnData("Change")
      )

      <.div(
        ^.onClick --> props.onClick,
        css.resourceDetailCard,
        <.div(css.cardTitleExpanded, card.name),
        <.div(
          css.driverInfo,
          <.div(css.driverInfoColumn, <.p(css.resetP, "Type"), <.p(css.resetP, "Status")),
          <.div(css.driverInfoColumnRight, <.p(css.resetP, card.`type`), ResourceWidget.driverStatus(card.status))
        ),
        Table(columnData, tableData),
        <.span(
          SPWidgetElements.buttonGroup(Seq(
            btn("Terminate Driver", onTerminateDriver(card.cardId)),
            btn("Start Driver", onStartDriver(card))
          ))
        )
      ).render
    }

    private def btn(text: String, onClick: Callback): VdomNode = {
      <.span(
        text,
        ^.onClick ==> (e => e.stopPropagationCB >> onClick),
        ^.className := "btn",
        SPWidgetElementsCSS.defaultMargin,
        SPWidgetElementsCSS.clickable,
        SPWidgetElementsCSS.button
      )
    }

    private def onStartDriver(card: DriverCard): Callback = Callback {
      DriverCommunication.postRequest(APIDeviceDriver.SetUpDeviceDriver(Driver(card.name, card.cardId, card.`type`, card.setup)))
    }

    private def onTerminateDriver(driverId: DriverId): Callback = Callback {
      DriverCommunication.postRequest(APIDeviceDriver.TerminateDriver(driverId))
    }

    private val component = ScalaComponent.builder[Props]("ResourceDetailView")
      .render_P(render)
      .configure(Reusability.shouldComponentUpdate)
      .build

    def apply(card: DriverCard, onClick: Callback): VdomElement = component(Props(card, onClick))
  }

  def createCorrectTypeOfSPValue(newValue: String): SPValue = { // Convert the incoming string to an SPvalue of the same type as the previous state value
    Try(SPValue(newValue.toInt))
      .orElse(Try(SPValue(newValue.toBoolean)))
      .getOrElse(SPValue(newValue))
  }

  def updateDriverState(card: DriverCard, s1: String)(e: ReactKeyboardEventFromInput): Callback = Callback {
    if (e.key == "Enter") {
      val association = s1 -> createCorrectTypeOfSPValue(e.target.value)
      DriverCommunication.postRequest(APIDeviceDriver.DriverCommand(card.cardId, card.state + association))
    }
  }
}