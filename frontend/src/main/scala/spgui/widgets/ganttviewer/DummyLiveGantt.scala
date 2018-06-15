package spgui.widgets.ganttviewer

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import sp.domain._
import sp.runners.{APIOperationRunner => oprapi}
import sp.abilityhandler.{APIAbilityHandler => ahapi}
import spgui.{SPWidget, SPWidgetBase}
import spgui.communication.APIComm.StreamHelper

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object DummyLiveGantt {

  // TODO runnerID should possibly be props
  case class State(runnerID: ID = null, abilityNames: Map[ID, String] = Map(), oprState: Map[ID, String] = Map())

  private class Backend($: BackendScope[SPWidgetBase, State]) {

    val oprComm = new OperationRunnerAPIComm(
      //stateEvent => println(stateEvent)
      stateEvent => {
        $.modState {
          s => s.copy(
            runnerID = stateEvent.runnerID,
            oprState = s.oprState ++ stateEvent.state.mapValues(_.toString)
          )
        }
      }.runNow()
    )

    val ahComm = new AbilityHandlerAPIComm

    def getAbilities(ids: Set[ID]) = {
      val abilitiesF = ahComm.request(ahapi.GetAbilities).takeFirstResponse.map(_._2).collect { case ahapi.Abilities(xs) => xs}
      abilitiesF.map(_.filter(a => ids.contains(a.id))) // TODO fetch only the abilities of interest instead of all of them
    }

    def getAbilityNames(runnerID: ID) = Callback.future {
      val runnerF = oprComm.request(oprapi.GetRunner(runnerID)).takeFirstResponse.map(_._2)
      val abilityIDsF = runnerF.map { case oprapi.Runner(setup) => setup.opAbilityMap.values.toSet }
      val abilityNamesF = abilityIDsF.flatMap(getAbilities)
      abilityNamesF.map(names => $.modState(s => s.copy(abilityNames = s.abilityNames ++ names.map(a => a.id -> a.name))))
    }

    def render(s: State) = {
      <.div(
        "Open VDTracker -> Create model \"DummyExample\" -> click both Launch buttons to get data",
        <.button("getRunner", ^.onClick --> getAbilityNames(s.runnerID)),
        <.div(s.abilityNames.mkString),
        <.ul(
          s.oprState.toTagMod { case (id, state) =>
            <.li(s.abilityNames.get(id).getOrElse(id.toString) + " state: ", state)
          }
        )
      )
    }

  }

  private val component = ScalaComponent.builder[SPWidgetBase]("DummyLiveGantt")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidUpdate { ctx =>
      val runnerID = ctx.currentState.runnerID
      if (ctx.prevState.runnerID == null && runnerID != null) ctx.backend.getAbilityNames(runnerID)
      else Callback.empty
    }
    .build

  def apply() = SPWidget(spwb => component(spwb))

}
