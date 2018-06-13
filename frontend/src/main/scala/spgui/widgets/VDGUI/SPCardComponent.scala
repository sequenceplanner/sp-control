package spgui.widgets.VDGUI
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._
import spgui.communication._

object SPCardGrid {
  case class State(expandedId: Option[ID] = None)
  case class Props(cards: List[RenderCard])

  trait RenderCard{val cardId: ID}
  case class DriverCard(cardId: ID, name: String, status: String, typ: String, state: List[(String, SPValue)]) extends RenderCard
  case class ResourceCard(cardId: ID, name: String, driverStatuses: List[(String, Boolean)], state: List[(String, SPValue)]) extends RenderCard


  class Backend($: BackendScope[Props, State]) {
    def render(p:Props, s: State) = {
      val isExpanded = !s.expandedId.isEmpty
      <.div(
        ^.className := DriverWidgetCSS.rootDiv.htmlClass,
        <.div(
          { isExpanded match {
            case true  => ^.className := DriverWidgetCSS.cardGroupExpanded.htmlClass
            case false => ^.className := DriverWidgetCSS.cardGroupCollapsed.htmlClass
          }},
          p.cards.map(
            c => c match {
              case dc: DriverCard => {
                val smallCard = driverCardSmall(dc)
                val expandedCard = driverCardExpanded(dc)
                renderCard(dc.cardId, s.expandedId, expandedCard, smallCard)
              }
              case rc: ResourceCard => {
                val smallCard = resourceCardSmall(rc)
                val expandedCard = resourceCardExpanded(rc)
                renderCard(rc.cardId, s.expandedId, expandedCard, smallCard)
              }
            }
          ).toTagMod
        )
      )
    }

    def renderCard(
      cardId: ID,
      expandedId: Option[ID],
      cardContentsExpanded: TagMod,
      cardContentsCollapsed: TagMod
    ): TagMod = {
      val isExpanded = expandedId == Some(cardId)
      List(
        <.span(
          ^.className := DriverWidgetCSS.cardPlaceholder.htmlClass,
          expandedId match {
            case None => EmptyVdom
            case _ =>
              if(expandedId == Some(cardId)) ^.className := DriverWidgetCSS.cardExpanded.htmlClass
              else ^.className := DriverWidgetCSS.cardCollapsed.htmlClass
          },
          <.span(
            ^.className := DriverWidgetCSS.cardOuter.htmlClass,
            expandedId match {
              case None => EmptyVdom
              case _ =>
                if(expandedId == Some(cardId)) ^.className := DriverWidgetCSS.unsetHeight.htmlClass
                else EmptyVdom
            },
            {
              isExpanded match {
                case true => cardContentsExpanded
                case false => cardContentsCollapsed
              }
            },
            ^.onClick --> $.modState(s =>
              if(s.expandedId == Some(cardId)) s.copy(expandedId = None)
              else s.copy(expandedId = Some(cardId))
            )
          )
        )
      ).toTagMod
    }
  }

  def driverCardSmall(card: DriverCard) = {
    <.div(
      ^.className := DriverWidgetCSS.driverCard.htmlClass,
      <.div(
        ^.className := DriverWidgetCSS.cardTitleSmall.htmlClass,
        card.name
      ),
      <.div(
        ^.className := DriverWidgetCSS.driverTypeSmall.htmlClass,
        <.div("Type: " + card.typ)
      ),
      <.div(
        ^.className := DriverWidgetCSS.driverStatusSmall.htmlClass,
        <.span("Status: "),
        <.span(
          card.status match {
            case "Online" => <.span(
              ^.className := DriverWidgetCSS.driverOnline.htmlClass,
              "Online"
            )
            case "Offline" => <.span(
              ^.className := DriverWidgetCSS.driverOffline.htmlClass,
              "Offline"
            )
            case "Unresponsive" => <.span(
              ^.className := DriverWidgetCSS.driverUnresponsive.htmlClass,
              "Unresponsive"
            )
            case s: String => <.span(s)
          }
        )
      )
    )
  }

  def driverCardExpanded(card: DriverCard) = {
    <.div(
      ^.className := DriverWidgetCSS.driverCard.htmlClass,
      <.div(
        ^.className := DriverWidgetCSS.cardTitleExpanded.htmlClass,
        card.name
      ),
      <.div(
        ^.className := DriverWidgetCSS.driverType.htmlClass,
        <.div("Type: " + card.typ)
      ),
      <.div(
        ^.className := DriverWidgetCSS.driverStatus.htmlClass,
        <.span("Status: "),
        <.span(
          card.status match {
            case "Online" => <.span(
              ^.className := DriverWidgetCSS.driverOnline.htmlClass,
              "Online"
            )
            case "Offline" => <.span(
              ^.className := DriverWidgetCSS.driverOffline.htmlClass,
              "Offline"
            )
            case "Unresponsive" => <.span(
              ^.className := DriverWidgetCSS.driverUnresponsive.htmlClass,
              "Unresponsive"
            )
            case s: String => <.span(s)
          }
        )
      ),
      <.div(
        ^.className := DriverWidgetCSS.driverStates.htmlClass,
        card.state.map( s =>
          <.div(s._1 + ": " + s._2)
        ).toTagMod
      )
    )
  }

  def resourceCardSmall(card: ResourceCard) = {
    <.div(
      ^.className := DriverWidgetCSS.resourceCard.htmlClass,
      <.div(
        ^.className := DriverWidgetCSS.cardTitleSmall.htmlClass,
        card.name
      ),
      <.div(
        card.driverStatuses.map{
          ds => <.div(
            ^.className := DriverWidgetCSS.driverStatus.htmlClass,
            <.span(ds._1),
            {
              val isActive = ds._2
              isActive match {
                case true => <.span(
                  ^.className := DriverWidgetCSS.driverOnline.htmlClass,
                  "Online"
                )
                case false => <.span(
                  ^.className := DriverWidgetCSS.driverOffline.htmlClass,
                  "Offline"
                )
              }
            }
          )
        }.toTagMod
      )
    )
  }

  def resourceCardExpanded(card: ResourceCard) = {
    <.div(
      ^.className := DriverWidgetCSS.resourceCard.htmlClass,
      <.div(
        ^.className := DriverWidgetCSS.cardTitleExpanded.htmlClass,
        card.name
      ),
      <.div(
        card.driverStatuses.map{
          s => s._1 + s._2.toString
        }.toTagMod
      ),
      <.div(
        ^.className := DriverWidgetCSS.stateList.htmlClass,
         card.state.map( s => 
          <.div(s._1 + ": " + s._2)
        ).toTagMod
      )
    )
  }

  private val component = ScalaComponent.builder[Props]("CardGrid")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(cards: List[RenderCard]) = component(Props(cards))
}
