package spgui.widgets.VDGUI
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._
import spgui.communication._

object SPCardGrid {
  case class State(expandedId: Option[ID] = None)
  case class Props(cards: List[RenderCard])

  trait RenderCard{val cardId: ID}
  case class DriverCard(cardId: ID, name: String, isOnline: Boolean, driverInfo: List[String], state: List[String]) extends RenderCard
  case class ResourceCard(cardId: ID, name: String, driverIds: List[ID], state: List[String]) extends RenderCard
  //case class OperationRunnerCard()


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
      <.div(
        ^.className := DriverWidgetCSS.cardTitle.htmlClass,
        card.name
      )
    )
  }

  def driverCardExpanded(card: DriverCard) = {
    <.div(
      <.div("testing id here also: " + card.cardId.toString),
      <.div(
        ^.className := DriverWidgetCSS.cardTitle.htmlClass,
        card.name
      ),
      <.div(
        card.driverInfo.map(<.div(_)).toTagMod
      ),
      <.div(
        card.state.map(<.div(_)).toTagMod
      )
    )
  }

  def resourceCardSmall(card: ResourceCard) = {
    <.div(
      <.div(
        ^.className := DriverWidgetCSS.cardTitle.htmlClass,
        card.name
      )
    )
  }

  def resourceCardExpanded(card: ResourceCard) = {
    <.div(
      <.div("testing id thing: " + card.driverIds.map{_.toString}.toString),
      <.div(
        ^.className := DriverWidgetCSS.cardTitle.htmlClass,
        card.name
      ),
      <.div(
        card.state.map(<.div(_)).toTagMod
      )
    )
  }

  private val component = ScalaComponent.builder[Props]("CardGrid")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(cards: List[RenderCard]) = component(Props(cards))
}
