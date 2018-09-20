package spgui.widgets.OPGUI

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import scalacss.internal.StyleA
import sp.domain._
import sp.domain.logic.{PropositionConditionLogic => PCL}
import spgui.SimpleSet
import spgui.circuits.main.handlers.{AbilityData, OperationData}

/** Card components for the OperationRunnerWidget */
object OperationRunnerCard {
  import spgui.widgets.OPGUI.{OperationRunnerWidgetCSS => css}
  @Lenses case class State(expandedId: Option[ID] = None)

  case class Props(modelIdAbles: SimpleSet[ID, IDAble], cards: List[CardData])

  class CardData(val operation: Option[OperationData], val ability: Option[AbilityData]) {
    val cardId: ID = (operation, ability) match {
      case (None, None) => throw new IllegalStateException("CardData must have either an ability or an operation.")
      case (None, Some(a)) => a.id
      case (Some(o), _) => o.id
    }
  }

  object CardData {
    def apply(ability: AbilityData): CardData = new CardData(None, Some(ability))
    def apply(operation: OperationData): CardData = new CardData(Some(operation), None)
    def apply(operation: OperationData, ability: AbilityData) = new CardData(Some(operation), Some(ability))
  }

  class Backend($: BackendScope[Props, State]) {
    def render(props: Props, state: State): VdomElement = {
      implicit def showProposition(p: Proposition): String = PCL.prettyPrint(props.modelIdAbles.toList)(p)

      val detailView = state.expandedId.isDefined
      def isExpanded(card: CardData) = state.expandedId.contains(card.cardId)

      val stateClass = if (detailView) css.cardGroupExpanded else css.cardGroupCollapsed
      val cards = {
        if (detailView) {
          props.cards
            .find(card => state.expandedId.contains(card.cardId))
            .map(card => renderCard(card, isExpanded(card)))
            .whenDefined
        }
        else props.cards.map(card => renderCard(card, isExpanded(card))).toTagMod
      }


      <.div(css.rootDiv, <.div(stateClass, cards)).render
    }

    def renderCard(card: CardData, expanded: Boolean)(implicit show: Proposition => String): TagMod = {
      val contents = if (expanded) cardExpanded(card) else cardSmall(card)

      <.span(
        css.cardPlaceholder,
        css.cardExpanded.when(expanded),
        <.span(
          css.cardOuter,
          css.unsetHeight.when(expanded),
          contents,
          ^.onClick --> $.modState(State.expandedId.set(if (expanded) None else Some(card.cardId)))
        )
      )
    }

    def cardSmall(card: CardData): TagMod = {
      def section(name: String, content: TagMod) = {
        <.span(
          css.smallOpOuter,
          <.div(
            css.smallOpInner,
            <.div(css.opNameOuter, <.div(css.smallOpName, name))
          ),
          content
        )
      }

      <.div(
        css.card,
        card.operation.map { operation =>
          section(operation.name, content = renderOperationState(operation.state))
        }.whenDefined,
        card.ability.map { ability =>
          section(ability.name, content = <.span(css.emphasizeText, ability.status.tag))
        }.whenDefined
      )
    }

    def cardExpanded(card: CardData)(implicit showProposition: Proposition => String): TagMod = {
      <.div(
        css.card,
        card.operation.map { operation =>
          <.span(
            css.opOuter,
            renderConditions(operation.name, operation.preConditions, operation.postConditions),
            renderOperationState(operation.state)
          )
        }.whenDefined,
        card.ability.map { ability =>
          <.span(
            css.opOuter,
            renderConditions(ability.name, List(ability.preCondition), List(ability.postCondition)),
            <.span(css.emphasizeText, ability.status.tag)
          )
        }.whenDefined
      )
    }

    private def renderConditions(conditions: Iterable[Condition], classTag: StyleA)(implicit show: Proposition => String): TagMod = {
      <.div(classTag, conditions.map(c => <.div(show(c.guard))).toTagMod).when(conditions nonEmpty)
    }

    def renderConditions(name: String, preConditions: Iterable[Condition], postConditions: Iterable[Condition])(implicit show: Proposition => String): TagMod = {
      <.div(
        css.opInner,
        renderConditions(preConditions, css.opPrecondition),
        <.div(css.opNameOuter, <.div(css.opName, name)),
        renderConditions(postConditions, css.opPostcondition)
      )
    }


    def renderOperationState(state: Map[ID, SPValue]): TagMod = {
      val renderMap = Map(
        "i" -> (css.green, "initialised"),
        "e" -> (css.orange, "executing"),
        "f" -> (css.blue, "finished")
      )

      val (present, unknown) = state.values
        .map(_.toString().filterNot(_ == '"'))
        .partition(renderMap.contains)

      val presentContent = present.map(renderMap).map { case (style, text) => <.span(style, text) }.toTagMod
      val unknownContent = unknown.map(<.span(_)).toTagMod

      <.span(css.emphasizeText, presentContent, unknownContent)
    }
  }

  private val component = ScalaComponent.builder[Props]("OperationRunnerCardComponent")
    .initialState(State())
    .renderBackend[Backend]
    .build

  /**
    * Allows syntax like {{{css.expandedCard}}} instead of {{{^.className := css.expandedCard.htmlClass}}}
    */
  implicit def toHtml(a: StyleA): TagMod = ^.className := a.htmlClass

  def apply(modelIdAbles: SimpleSet[ID, IDAble], cards: Iterable[CardData]): VdomElement = {
    component(Props(modelIdAbles, cards.toList))
  }
}
