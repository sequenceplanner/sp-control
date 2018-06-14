package spgui.widgets.ganttviewer

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import sp.domain._
import spgui.SPWidget
import spgui.SPWidgetBase


object DummyLiveGantt {

  case class State(oprState: Map[ID, String] = Map())

  private class Backend($: BackendScope[SPWidgetBase, State]) {

    val oprComm = new OperationRunnerAPIComm(
      //stateEvent => println(stateEvent)
      stateEvent => $.modState(s => s.copy(oprState = s.oprState ++ stateEvent.state.mapValues(_.toString))).runNow()
    )

    def render(s: State) =
      <.div(
        "hej from DummyLiveGantt",
        <.ul(
          s.oprState.toTagMod { case (id, state) =>
            <.li(id.toString + " state: ", state)
          }
        )
      )

  }

  private val component = ScalaComponent.builder[SPWidgetBase]("DummyLiveGantt")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply() = SPWidget(spwb => component(spwb))

}
