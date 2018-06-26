package spgui.widgets.OPGUI
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._
import spgui.communication._
import sp.devicehandler.APIDeviceDriver
import japgolly.scalajs.react.vdom.all.svg

import scala.scalajs.js
import sp.domain.logic.{PropositionConditionLogic => PCL}
import spgui.widgets.OPGUI.OperationRunnerWidget.{AbilityWithState, OperationWithState}

/** Cardcomponents for the OperationRunnerWidget */
object OperationRunnerCardComponent {
  case class State(expandedId: Option[ID] = None)
  case class Props(modelIdables: List[IDAble], cards: List[RunnerCard])

  trait RunnerCard
  case class OperationRunnerLonelyOp(lonelyCardId: ID, op: OperationRunnerWidget.OperationWithState) extends RunnerCard
  case class OperationRunnerLonelyAb(lonelyCardId: ID, ab: OperationRunnerWidget.AbilityWithState) extends RunnerCard
  case class OperationRunnerCard(cardId: ID, ab: AbilityWithState, op: OperationWithState) extends RunnerCard
  case class AvailableCard(cardId: ID, ab: AbilityWithState, op: OperationWithState) extends RunnerCard


  class Backend($: BackendScope[Props, State]) {
    def render(p:Props, s: State) = {
      val propositionPrinter = PCL.prettyPrint(p.modelIdables)_

      val isExpanded = s.expandedId.isDefined
      <.div(
        ^.className := OperationRunnerWidgetCSS.rootDiv.htmlClass,
        <.div(
          { isExpanded match {
            case true  => ^.className := OperationRunnerWidgetCSS.cardGroupExpanded.htmlClass
            case false => ^.className := OperationRunnerWidgetCSS.cardGroupCollapsed.htmlClass
          }},
          p.cards.map{
            card =>
              card match {
                case opab: OperationRunnerCard => {
                  val smallCard = cardSmall(opab, propositionPrinter)
                  val expandedCard = cardExpanded(opab, propositionPrinter)
                  <.div(renderCard(opab.cardId, s.expandedId, expandedCard, smallCard))
                }
                case op: OperationRunnerLonelyOp => {
                  val smallCard = lonelyCardSmall(op, propositionPrinter)
                  val expandedCard = lonelyCardExpanded(op, propositionPrinter)
                  <.div(renderCard(op.lonelyCardId, s.expandedId, expandedCard, smallCard))
                }
                case ab: OperationRunnerLonelyAb => {
                  val smallCard = lonelyCardSmall(ab, propositionPrinter)
                  val expandedCard = lonelyCardExpanded(ab, propositionPrinter)
                  <.div(renderCard(ab.lonelyCardId, s.expandedId, expandedCard, smallCard))
                }
                case available: AvailableCard =>
                  val smallCard = smallAvailableCard(available, propositionPrinter)
                  val expandedCard = expandedAvailableCard(available, propositionPrinter)
                  <.div(renderCard(available.cardId, s.expandedId, expandedCard, smallCard))
                case _ => <.div()
              }
          }.toTagMod
        )
      )
    }


    import sp.domain.logic.AttributeLogic._
    def cardSmall(opab: OperationRunnerCard, printer: (Proposition) => String) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.card.htmlClass,
        <.span(
          ^.className := OperationRunnerWidgetCSS.smallOpOuter.htmlClass,
          renderSmallOp(
            opab.op.operation.name, List(), List(), getOpState(opab.op.operationState) 
          ),
          renderOpState(opab.op.operationState)
        ),
        <.span(
          ^.className := OperationRunnerWidgetCSS.smallOpOuter.htmlClass,
          renderSmallOp(
            opab.ab.ability.name
          ),
          renderAbState(opab.ab.abilityState)
        )
      )
    }

    def cardExpanded(opab: OperationRunnerCard, printer: (Proposition) => String) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.card.htmlClass,
        <.span(
          ^.className := OperationRunnerWidgetCSS.opOuter.htmlClass,
          renderOp(
            opab.op.operation.name,
            opab.op.operation.conditions.map(c =>
              c.attributes.getAs[String]("kind").collect{case "pre" => printer(c.guard)}).flatten,
            opab.op.operation.conditions.map(c =>
              c.attributes.getAs[String]("kind").collect{case "post" => printer(c.guard)}).flatten
          ),
          renderOpState(opab.op.operationState)
        ),
        <.span(
          ^.className := OperationRunnerWidgetCSS.opOuter.htmlClass,
          renderOp(
            opab.ab.ability.name,
            List(printer(opab.ab.ability.preCondition.guard)),
            List(printer(opab.ab.ability.postCondition.guard))
          ),
          renderAbState(opab.ab.abilityState)
        )
      )
    }

    def smallAvailableCard(availableCard: AvailableCard, propositionToString: (Proposition) => String) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.card.htmlClass,
        <.span(
          ^.className := OperationRunnerWidgetCSS.smallOpOuter.htmlClass,
          renderSmallOp(
            "Available " + availableCard.op.operation.name, List(), List(), getOpState(availableCard.op.operationState)
          ),
          renderOpState(availableCard.op.operationState)
        ),
        <.span(
          ^.className := OperationRunnerWidgetCSS.smallOpOuter.htmlClass,
          renderSmallOp(
            "Available " + availableCard.ab.ability.name
          ),
          renderAbState(availableCard.ab.abilityState)
        )
      )
    }

    def expandedAvailableCard(availableCard: AvailableCard, printer: (Proposition) => String) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.card.htmlClass,
        <.span(
          ^.className := OperationRunnerWidgetCSS.opOuter.htmlClass,
          renderOp(
            "Available Operation: " + availableCard.op.operation.name,
            availableCard.op.operation.conditions.map(c =>
              c.attributes.getAs[String]("kind").collect{case "pre" => printer(c.guard)}).flatten,
            availableCard.op.operation.conditions.map(c =>
              c.attributes.getAs[String]("kind").collect{case "post" => printer(c.guard)}).flatten
          ),
          renderOpState(availableCard.op.operationState)
        ),
        <.span(
          ^.className := OperationRunnerWidgetCSS.opOuter.htmlClass,
          renderOp(
            "Available Ability: " + availableCard.ab.ability.name,
            List(printer(availableCard.ab.ability.preCondition.guard)),
            List(printer(availableCard.ab.ability.postCondition.guard))
          ),
          renderAbState(availableCard.ab.abilityState)
        )
      )
    }

    def lonelyCardSmall(operation: OperationRunnerLonelyOp, printer: (Proposition) => String) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.card.htmlClass,
        <.span(
          ^.className := OperationRunnerWidgetCSS.smallOpOuter.htmlClass,
          renderSmallOp(
            "OP: " + operation.op.operation.name
          ),
          renderOpState(operation.op.operationState)
        )
      )
    }
    def lonelyCardSmall(ability: OperationRunnerLonelyAb, printer: (Proposition) => String) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.card.htmlClass,
        <.span(
          ^.className := OperationRunnerWidgetCSS.smallOpOuter.htmlClass,
          renderSmallOp(
            "AB: " + ability.ab.ability.name
          ),
          renderOpState(ability.ab.abilityState)
        )
      )
    }
    def lonelyCardExpanded(operation: OperationRunnerLonelyOp, printer: (Proposition) => String) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.card.htmlClass,
        <.span(
          ^.className := OperationRunnerWidgetCSS.opOuter.htmlClass,
          renderOp(
            "OP: " + operation.op.operation.name,
            operation.op.operation.conditions.map(c =>
              c.attributes.getAs[String]("kind").collect{case "pre" => printer(c.guard)}).flatten,
            operation.op.operation.conditions.map(c =>
              c.attributes.getAs[String]("kind").collect{case "post" => printer(c.guard)}).flatten
          ),
          renderOpState(operation.op.operationState)
        )
      )
    }
    def lonelyCardExpanded(ability: OperationRunnerLonelyAb, printer: (Proposition) => String) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.card.htmlClass,
        <.span(
          ^.className := OperationRunnerWidgetCSS.opOuter.htmlClass,
          renderOp(
            "AB: " + ability.ab.ability.name,
            List(printer(ability.ab.ability.preCondition.guard)),
            List(printer(ability.ab.ability.postCondition.guard))
          ),
          renderAbState(ability.ab.abilityState)
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

  def renderOp(name: String, preConditions: List[String] = List(), postConditions: List[String] = List()) = {
    <.div(
      ^.className := OperationRunnerWidgetCSS.opInner.htmlClass,
      {
        if(preConditions.isEmpty) {
          EmptyVdom
        } else {
          <.div(
            ^.className := OperationRunnerWidgetCSS.opPrecondition.htmlClass,
            preConditions.map(
              pre => <.div(pre)
            ).toTagMod
          )
        }
      },
      <.div(
        ^.className := OperationRunnerWidgetCSS.opNameOuter.htmlClass,
        <.div(
          ^.className := OperationRunnerWidgetCSS.opName.htmlClass,
          name
        )
      ),
      {
        if(postConditions.isEmpty) {
          EmptyVdom
        } else {
          <.div(
            ^.className := OperationRunnerWidgetCSS.opPostcondition.htmlClass,
            postConditions.map(
              post => <.div(post)
            ).toTagMod
          )
        }
      }
    )
  }


  import spgui.widgets.sopmaker.SopMakerCSS
  val opHeight = 50
  val opWidth = 60
  def renderSmallOp(
    name: String,
    preConditions: List[String] = List(),
    postConditions: List[String] = List(),
    state: String = "NoneState"
  ) = {
    <.span(
      <.div(
        ^.className := SopMakerCSS.opInner.htmlClass,
        <.div(
          ^.className := SopMakerCSS.preCondition.htmlClass
        ),
        <.div(
          ^.className := SopMakerCSS.opNameOuter.htmlClass,
          {
            state match {
              case "i" => ^.className := SopMakerCSS.opStateInit.htmlClass
              case "e" => ^.className := SopMakerCSS.opStateExec.htmlClass
              case "f" => ^.className := SopMakerCSS.opStateFini.htmlClass
              case _ => ^.className := SopMakerCSS.opStateNone.htmlClass
            }
          },
          <.div(
            ^.className := SopMakerCSS.opName.htmlClass,
            name
          )
        ),
        <.div(
          ^.className := SopMakerCSS.postCondition.htmlClass
        )
      )
    )
  }


  // def renderSmallOp(name: String, preConditions: List[String] = List(), postConditions: List[String] = List()) = {
  //   <.div(
  //     ^.className := OperationRunnerWidgetCSS.smallOpInner.htmlClass,
  //     {
  //       if(preConditions.isEmpty) {
  //         EmptyVdom
  //       } else {
  //         <.div(
  //           ^.className := OperationRunnerWidgetCSS.opPrecondition.htmlClass,
  //           preConditions.map(
  //             pre => <.div(pre)
  //           ).toTagMod
  //         )
  //       }
  //     },
  //     <.div(
  //       ^.className := OperationRunnerWidgetCSS.opNameOuter.htmlClass,
  //       <.div(
  //         ^.className := OperationRunnerWidgetCSS.smallOpName.htmlClass,
  //         name
  //       )
  //     ),
  //     {
  //       if(postConditions.isEmpty) {
  //         EmptyVdom
  //       } else {
  //         <.div(
  //           ^.className := OperationRunnerWidgetCSS.opPostcondition.htmlClass,
  //           postConditions.map(
  //             post => <.div(post)
  //           ).toTagMod
  //         )
  //       }
  //     }
  //   )
  // }

  def getOpState(state: Map[ID, SPValue]) = {
    state.map{case (key, value) => {
      value.toString match {
        case "\"i\"" => "i"
        case "\"e\"" => "e"
        case "\"f\"" => "f"
        case _ => "NoneState"
      }
    }}.head
  }

  def renderOpState(state: Map[ID, SPValue]) = {
    <.span(
      ^.className := OperationRunnerWidgetCSS.opabState.htmlClass,
      state.map{case (key, value) => {
        value.toString match {
          case "\"i\"" => <.span(
            ^.className := OperationRunnerWidgetCSS.opStateInit.htmlClass,
            "initialised"
          )
          case "\"e\"" => <.span(
            ^.className := OperationRunnerWidgetCSS.opStateExec.htmlClass,
            "executing"
          )
          case "\"f\"" => <.span(
            ^.className := OperationRunnerWidgetCSS.opStateFini.htmlClass,
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

  object AbilityStateEnum {
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
          case AbilityStateEnum.unavailable => "unavailable"
          case AbilityStateEnum.notEnabled => "notEnabled"
          case AbilityStateEnum.enabled => "enabled"
          case AbilityStateEnum.starting => "starting"
          case AbilityStateEnum.executing => "executing"
          case AbilityStateEnum.finished => "finished"
          case AbilityStateEnum.forcedReset => "forcedReset"
          case AbilityStateEnum.failed => "failed"
          case "invalidState" => "invalidState"
        }
      }.toTagMod
    )
  }

  private val component = ScalaComponent.builder[Props]("OperationAbilityCardGrid")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply(modelIdables: List[IDAble], cards: List[RunnerCard]) = component(Props(modelIdables, cards))
}
