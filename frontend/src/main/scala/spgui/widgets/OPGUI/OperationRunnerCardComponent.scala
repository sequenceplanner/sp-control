package spgui.widgets.OPGUI
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._
import spgui.communication._
import sp.devicehandler.APIDeviceDriver

object SPCardGrid {
  case class State(expandedId: Option[ID] = None)
  case class Props(cards: List[RenderCard])

  case class RenderCard(cardId: ID, op: RenderOperation, ab: RenderAbility)
  case class RenderOperation(name: String)
  case class RenderAbility(name: String)

  class Backend($: BackendScope[Props, State]) {
    def render(p:Props, s: State) = {
      val isExpanded = s.expandedId.isDefined
      <.div(
        ^.className := OperationRunnerWidgetCSS.rootDiv.htmlClass,
        <.div(
          { isExpanded match {
            case true  => ^.className := OperationRunnerWidgetCSS.cardGroupExpanded.htmlClass
            case false => ^.className := OperationRunnerWidgetCSS.cardGroupCollapsed.htmlClass
          }},
          p.cards.map(
            c => c match {
              case opab: RenderCard => {
                val smallCard = cardSmall(opab)
                val expandedCard = cardExpanded(opab)
                renderCard(opab.cardId, s.expandedId, expandedCard, smallCard)
              }
            }
          ).toTagMod
        )
      )
    }
    
    def cardSmall(opab: RenderCard) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.card.htmlClass,
        <.span(
          opab.op.name
        ),
        <.span(
          opab.ab.name
        )
      )
    }


    def cardExpanded(opab: RenderCard) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.card.htmlClass,
        <.span(
          opab.op.name
        ),
        <.span(
          opab.ab.name
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
          ^.className := OperationRunnerWidgetCSS.cardPlaceholder.htmlClass,
          expandedId match {
            case None => EmptyVdom
            case _ =>
              if(expandedId == Some(cardId)) ^.className := OperationRunnerWidgetCSS.cardExpanded.htmlClass
              else ^.className := OperationRunnerWidgetCSS.cardCollapsed.htmlClass
          },
          <.span(
            ^.className := OperationRunnerWidgetCSS.cardOuter.htmlClass,
            expandedId match {
              case None => EmptyVdom
              case _ =>
                if(expandedId == Some(cardId)) ^.className := OperationRunnerWidgetCSS.unsetHeight.htmlClass
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

  private val component = ScalaComponent.builder[Props]("OperationAbilityCardGrid")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(cards: List[RenderCard]) = component(Props(cards))
}
