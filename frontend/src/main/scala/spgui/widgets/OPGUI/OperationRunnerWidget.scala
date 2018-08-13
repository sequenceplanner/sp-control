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
import sp.domain.Logic._


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
      val cardData: (OperationId, AbilityId) => Option[CardData] = (operationId, abilityId) => {
        for {
          ability <- props.abilities.get(abilityId)
          operation <- props.activeRunner.map(_.operations).flatMap(_.get(operationId))
        } yield CardData(operation, ability)
      }

      val associationCards = props.activeRunner
        .map(_.associations.toList)
        .toList
        .flatten
        .map(cardData.tupled)
        .flatten

      val operationCards = props.modelIdAbles
          .filter { case (_, v) => v.attributes.keys.contains("domain") }
          .flatMap { thing => thing.attributes.getAs[Set[Operation]]("ops").getOrElse(Set()) }
          .filterNot(o => associationCards.exists(_.operation.exists(_.id == o.id)))
          .flatMap(o => props.activeRunner.map(_.operations).flatMap(_.get(o.id)))
          .map(operationData => CardData(operationData))

      val abilityCards = props.abilities
        .filter { case (_, ability) => !associationCards.exists(_.ability.exists(_.id == ability.id)) }
        .map(CardData(_))


      <.div(
        ^.className := css.widgetRoot.htmlClass,
        OperationRunnerCard(props.modelIdAbles, associationCards ++ operationCards ++ abilityCards)

      ).render
    }
  }

  private val component = ScalaComponent.builder[Props]("OperationRunnerWidget")
    .renderBackend[Backend]
    .build

  private val connect = MainCircuit.connectComponent(identity)

  def apply() = SPWidget(_ => connect { proxy => component(Props(proxy))})
}
