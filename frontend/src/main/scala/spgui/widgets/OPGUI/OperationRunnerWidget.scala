package spgui.widgets.OPGUI

import diode.react.ModelProxy
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._
import spgui.circuits.main.handlers.{AbilityData, Runner}
import spgui.{SPWidget, SimpleSet}
import spgui.circuits.main.{FrontendState, MainCircuit}
import spgui.circuits.main.handlers.Aliases.{AbilityId, OperationId, RunnerId}
import spgui.widgets.OPGUI.OperationRunnerCard.CardData

/** Widget to visualize the pairs of abilities/operations */
object OperationRunnerWidget {
  import spgui.widgets.OPGUI.{OperationRunnerWidgetCSS => css}
  case class Props(proxy: ModelProxy[FrontendState]) {
    def modelIdAbles: SimpleSet[ID, IDAble] = proxy.value.models.activeModel.fold(SimpleSet[ID, IDAble](_.id))(_.items)

    def activeRunner: Option[Runner] = {
      for {
        runnerId <- proxy.value.virtualDevices.latestActiveRunnerId
        runner <- proxy.value.runners.runners.get(runnerId)
      } yield runner
    }

    def abilities: SimpleSet[AbilityId, AbilityData] = proxy.value.abilities.abilities
    def runners: SimpleSet[RunnerId, Runner] = proxy.value.runners.runners
  }

  private class Backend($: BackendScope[Props, Unit]) {
    def render(props: Props) = {
      val renderCard: (OperationId, AbilityId) => Option[CardData] = (operationId, abilityId) => {
        for {
          ability <- props.abilities.get(abilityId)
          operation <- props.activeRunner.map(_.operations).flatMap(_.get(operationId))
        } yield CardData(operationId, ability, operation)
      }

      val cards = props.activeRunner
        .map(_.associations.toList)
        .toList
        .flatten
        .map(renderCard.tupled)
        .flatten

      //

      <.div(
        ^.className := css.widgetRoot.htmlClass,
        OperationRunnerCard(props.modelIdAbles, cards)
        /*
        OperationRunnerCardComponent(
          state.modelIdables, {
            val opAbCards: List[RunnerCard] = state.activeOpAbPairs.map { operationAbilityPair => {
              val op = state.operationStateMapper(operationAbilityPair.operationID)
              val ab = state.abilityStateMapper(operationAbilityPair.abilityID)
              OperationRunnerCardComponent.OperationRunnerCard(op.operation.id, ab, op)
            }
            }
            val things = state.modelIdables.collect{ case t: Thing if t.attributes.keys.contains("domain") => t}
            val lonelyOperationMap: Map[ID, OperationWithState] =
              state.operationStateMapper.filter{operationWithState => !state.operationAbilityMap.contains(operationWithState._1)}
            println(s"Lonely Operations: $lonelyOperationMap \nStateMapper: ${state.operationStateMapper} " +
              s"\nOperationThings: $things")
            val lonelyOperations: List[OperationWithState] = lonelyOperationMap.values.toList
            val lonelyCards: List[RunnerCard] = lonelyOperations.map{lonelyOp =>
              OperationRunnerCardComponent.OperationRunnerLonelyOp(lonelyOp.operation.id, lonelyOp)
            }
            val mergeCards: List[RunnerCard] = opAbCards ++ lonelyCards
            mergeCards
          }
        )
        */
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("OperationRunnerWidget")
    .renderBackend[Backend]
    .build

  private val connect = MainCircuit.connectComponent(identity)

  def apply() = SPWidget(_ => connect { proxy => component(Props(proxy))})
}
