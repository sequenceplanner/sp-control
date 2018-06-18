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
  case class State(
                    runnerID: ID = null,
                    abilityNames: Map[ID, String] = Map(), // operation ID -> ability name
                    oprState: Map[ID, String] = Map()
                  )

  private class Backend($: BackendScope[SPWidgetBase, State]) {

    val oprComm = new OperationRunnerAPIComm(
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

    def getAbilities(opAbMap: Map[ID, ID]) = {
      val abilitiesF = ahComm.request(ahapi.GetAbilities).takeFirstResponse.map(_._2).collect { case ahapi.Abilities(xs) => xs}
      // TODO fetch only the abilities of interest instead of all of them
      abilitiesF.map(_.foldLeft(Map[ID, String]()) { (map, ability) =>
        val m = opAbMap.find(t => t._2 == ability.id)
        if(m.isDefined) map + (m.get._1 -> ability.name)
        else map
      })
    }

    def getRunnerSetup(runnerID: ID) = {
      oprComm.request(oprapi.GetRunner(runnerID)).takeFirstResponse.map(_._2).collect { case oprapi.Runner(setup) => setup}
    }

    def getAbilityNames(runnerID: ID) = Callback.future {
      val opAbMapF = getRunnerSetup(runnerID).map(_.opAbilityMap)
      val abilitiesF = opAbMapF.flatMap(getAbilities)
      abilitiesF.map(abs => $.modState(s => s.copy(abilityNames = s.abilityNames ++ abs)))
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
