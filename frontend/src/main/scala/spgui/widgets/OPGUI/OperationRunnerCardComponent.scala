package spgui.widgets.OPGUI
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._
import spgui.communication._
import sp.devicehandler.APIDeviceDriver
import japgolly.scalajs.react.vdom.all.svg
import scala.scalajs.js

object SPCardGrid {
  case class State(expandedId: Option[ID] = None)
  case class Props(cards: List[OperationRunnerCard])

  case class OperationRunnerCard(cardId: ID, ab: OperationRunnerWidget.AbilityWithState, op: OperationRunnerWidget.OperationWithState)

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
              case opab: OperationRunnerCard => {
                val smallCard = cardSmall(opab)
                val expandedCard = cardExpanded(opab)
                renderCard(opab.cardId, s.expandedId, expandedCard, smallCard)
              }
            }
          ).toTagMod
        )
      )
    }


    def cardSmall(opab: OperationRunnerCard) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.card.htmlClass,
        <.span(
          ^.className := OperationRunnerWidgetCSS.opOuter.htmlClass,
          renderOp(
            opab.op.operation.name// ,
            // Some(opab.op.operation.preCondition.toString),
            // Some(opab.op.operation.postCondition.toString)
          ),
          renderOpState(opab.op.operationState)
        ),
        <.span(
          ^.className := OperationRunnerWidgetCSS.opOuter.htmlClass,
          renderOp(
            opab.ab.ability.name,
            Some("precondition"),
            Some("postCondition")
     //       Some(opab.ab.ability.preCondition.toString),
      //      Some(opab.ab.ability.postCondition.toString)
          ),
          renderAbState(opab.ab.abilityState)
        )
      )
    }

    def cardExpanded(opab: OperationRunnerCard) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.card.htmlClass,
        <.span(
          ^.className := OperationRunnerWidgetCSS.opOuter.htmlClass,
          renderOp(opab.op.operation.name,Some("hello"), Some("goodbye")),
          renderOpState(opab.op.operationState)
        ),
        <.span(
          ^.className := OperationRunnerWidgetCSS.opOuter.htmlClass,
          renderOp(opab.ab.ability.name),
          renderAbState(opab.ab.abilityState)
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

  def renderOp(name: String, preCondition: Option[String] = None, postCondition: Option[String] = None) = {
    <.div(
      ^.className := OperationRunnerWidgetCSS.opInner.htmlClass,
      preCondition.map(pre => {
        <.div(
          ^.className := OperationRunnerWidgetCSS.opPrecondition.htmlClass,
          pre
        )
      }).getOrElse(EmptyVdom),
      <.div(
        ^.className := OperationRunnerWidgetCSS.opNameOuter.htmlClass,
        <.div(
          ^.className := OperationRunnerWidgetCSS.opName.htmlClass,
          name
        )
      ),
      postCondition.map(post => {
        <.div(
          ^.className := OperationRunnerWidgetCSS.opPostcondition.htmlClass,
          post
        )
      }).getOrElse(EmptyVdom)
    )
  }


  def renderOpState(state: Map[ID, SPValue]) = {
    <.span(
      ^.className := OperationRunnerWidgetCSS.opabState.htmlClass,
      state.map{case (key, value) => {
        value.toString match {
          case "\"i\"" => <.span(
            ^.className := OperationRunnerWidgetCSS.green.htmlClass,
            "initialised"
          )
          case "\"e\"" => <.span(
            ^.className := OperationRunnerWidgetCSS.spOrange.htmlClass,
            "executing"
          )
          case "\"f\"" => <.span(
            ^.className := OperationRunnerWidgetCSS.blue.htmlClass,
            "finished"
          )
          case _ => <.span(value.toString)
        }
      }}.toTagMod
    )
  }

  import play.api.libs.json._
  case class abilityState(state: String, counter: Int)
  implicit val fAbState: JSFormat[abilityState] = Json.format[abilityState]

  object AbilityState {
    val unavailable = "unavailable"
    val notEnabled = "notEnabled"
    val enabled = "enabled"
    val starting = "starting"
    val executing = "executing"
    val finished = "finished"
    val forcedReset = "forcedReset"
    val failed = "failed"
  }

  def renderAbState(state: Map[ID, SPValue]) = {
    <.span(
      ^.className := OperationRunnerWidgetCSS.opabState.htmlClass,
      state.map{case (key, value) => {
        value.asOpt[abilityState].map{ abState =>
          abState.state
        }.getOrElse("invalidState")
      }}.map{s =>
        s match {
          case AbilityState.unavailable => "unavailable"
          case AbilityState.notEnabled => "notEnabled"
          case AbilityState.enabled => "enabled"
          case AbilityState.starting => "starting"
          case AbilityState.executing => "executing"
          case AbilityState.finished => "finished"
          case AbilityState.forcedReset => "forcedReset"
          case AbilityState.failed => "failed"
          case "invalidState" => "invalidState"
        }        
      }.toTagMod
    )
  }

  private val component = ScalaComponent.builder[Props]("OperationAbilityCardGrid")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(cards: List[OperationRunnerCard]) = component(Props(cards))
}
