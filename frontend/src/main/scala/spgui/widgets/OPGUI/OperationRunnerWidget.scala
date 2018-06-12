package spgui.widgets.OPGUI

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.abilityhandler._
import sp.domain._
import spgui.communication._

object OperationRunnerWidget {

  // In OperationRunnerWidget, we want to visualize the pairs of abilites/operations
  case class OpAbPair(ability: APIAbilityHandler.Ability, operation: Operation)

  // we need to seperate the activeCards (the pairs the runner is using)
  // and the Operation/Ability-pair available, which we later can activate to the runner
  case class State(
                    activeOpAbPairs:    List[OpAbPair], // in the runner
                    availableOpAbPairs: List[OpAbPair]  //
                  )

  private class Backend($: BackendScope[Unit, State]) {


    def onUnmount() = {
      println("AbilityWidget Unmouting")
      Callback.empty
    }
  }

  private val driverWidgetComponent = ScalaComponent.builder[Unit]("AbilityWidget")
    .initialState(State(List(), List()))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => driverWidgetComponent())
}





