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
    def render(props: Props): VdomElement = {
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
              val op = state.operationStateMapper(operationAbilityPair._2)
              val ab = state.abilityStateMapper(operationAbilityPair._1)
              OperationRunnerCardComponent.OperationRunnerCard(op.operation.id, ab, op)
            }
            }.toList
            // TODO: Fix the backend issue with operations as things and no information about the state of the operation
            val operationThings = state.modelIdables.filter{_.attributes.keys.contains("domain")}
            val operations = operationThings.map{opThing =>
              opThing.attributes.getAs[Set[Operation]]("ops").getOrElse(Set())
            }.flatten
            val unusedOperations = operations.filterNot{op =>
              state.activeOpAbPairs.contains(op.id)
            }
            val lonelyOpCards = unusedOperations.map{op =>
              OperationRunnerCardComponent.OperationRunnerLonelyOp(
                op.id,
                OperationWithState(op, Map())
              )
            }
//            val operations = state.modelIdables.collect {case o: Operation => o}
            /*println(s"Things: $things \n" +
              s"Operations: $operations")*/
           /* val lonelyThings = operationThings.filterNot{opThing => state.operationAbilityMap.contains(opThing.id)}
            val lonelyOperationMap: Map[ID, OperationWithState] =
              state.operationStateMapper.filterNot{operationWithState => state.operationAbilityMap.contains(operationWithState._1)}

            */
           /* val lonelyOpCards: List[RunnerCard] = lonelyThings.map{thing =>
              val newOperation = Operation(name = thing.name,conditions = List(), attributes = thing.attributes, id = thing.id )
              OperationRunnerCardComponent.OperationRunnerLonelyOp(
                thing.id,
                OperationWithState(newOperation, Map())
              )

            }*/

            val lonelyAbilityMap = state.abilityStateMapper.filterNot{ability =>
              state.activeOpAbPairs.contains(ability._1)
            }

            val lonelyAbCards: List[RunnerCard] = lonelyAbilityMap.map{ab =>
              OperationRunnerCardComponent.OperationRunnerLonelyAb(
                ab._1, ab._2
              )
            }.toList

            val mergeCards: List[RunnerCard] = opAbCards ++ lonelyOpCards ++ lonelyAbCards
            mergeCards
          }
        )
        */
      ).render
    }
  }

  private val component = ScalaComponent.builder[Props]("OperationRunnerWidget")
    .renderBackend[Backend]
    .build

  private val connect = MainCircuit.connectComponent(identity)

  def apply() = SPWidget(_ => connect { proxy => component(Props(proxy))})
}
