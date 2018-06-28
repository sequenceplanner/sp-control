package spgui.widgets.VDGUI.cards

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}
import japgolly.scalajs.react.extra.Reusability
import play.api.libs.json.JsValue
import sp.domain.{ID, SPValue}
import spgui.circuits.main.handlers.Aliases.DriverName
import spgui.circuits.main.handlers.DriverHandler.DriverStatus
import spgui.widgets.VDGUI.Table
import spgui.widgets.VDGUI.ResourceWidget.{jsValueToString, renderDriverStatus}
import spgui.widgets.VDGUI.Table.ColumnData

import spgui.widgets.VDGUI.cards.{CardViewCSS => css}
import spgui.widgets.VDGUI.CSSHelpers.toHtml

object ResourceView {
  case class ResourceCard(cardId: ID, name: String, driverStatuses: List[(DriverName, DriverStatus)], state: List[(String, SPValue)])

  object Overview {
    case class Props(cards: List[ResourceCard], onCardClick: ID => Callback)

    implicit val cardReusability: Reusability[ResourceCard] = Reusability.by(_.driverStatuses)
    implicit val propsReusability: Reusability[Props] = Reusability.by(_.cards)

    def resourceOverviewCard(props: Props, card: ResourceCard): TagMod = {
      <.div(
        css.overviewCard,
        ^.onClick --> props.onCardClick(card.cardId),
        <.p(css.cardTitleSmall, card.name),
        renderDriverStatus(card.driverStatuses)
      )
    }

    def render(props: Props): VdomElement = {
      <.div(props.cards.map(card => resourceOverviewCard(props, card)).toTagMod)
    }

    private val component = ScalaComponent.builder[Props]("ResourceOverviewView")
      .render_P(render)
      .configure(Reusability.shouldComponentUpdate)
      .build

    def apply(cards: List[ResourceCard], onCardClick: ID => Callback): VdomElement = component(Props(cards, onCardClick))
  }

  object Detail {

    case class Props(card: ResourceCard, onClick: Callback)

    implicit val jsValueReusability: Reusability[JsValue] = Reusability.by(_.toString)
    implicit val hmReuse: Reusability[(String, JsValue)] = Reusability.tuple2
    implicit val stateReusability: Reusability[List[(String, JsValue)]] = Reusability.byIterator
    implicit val cardReusability: Reusability[ResourceCard] = Reusability.derive
    implicit val reusability: Reusability[Props] = Reusability.by((props: Props) => props.card)

    private def render(props: Props): VdomElement = {
      val columnData = Vector(
        ColumnData("Name"),
        ColumnData("Value", css.center)
      )
      val data = props.card.state.map { case (k, v) => (k, jsValueToString(v)) }

      <.div(
        css.resourceDetailCard,
        ^.onClick --> props.onClick,
        <.div(css.cardTitleExpanded, props.card.name),
        <.div(css.spacing, renderDriverStatus(props.card.driverStatuses)),
        Table(columnData, data, props.onClick)
      ).render
    }

    private val component = ScalaComponent.builder[Props]("ResourceDetailView")
      .render_P(render)
      .configure(Reusability.shouldComponentUpdate)
      .build

    def apply(card: ResourceCard, onClick: Callback): VdomElement = component(Props(card, onClick))
  }
}
