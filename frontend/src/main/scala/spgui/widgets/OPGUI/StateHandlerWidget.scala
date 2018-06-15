package spgui.widgets.StateHandlerWidget

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.TagOf
import org.scalajs.dom.html

import sp.domain._
import sp.domain.Logic._

object StateHandlerWidget {
  // Case class for a operation and its state
  case class OperationWithState(operation: Operation, operationState: Map[ID, SPValue])
  case class State(
                    activeRunnerID:       Option[ID] = None,
                    operationStateMapper: Map[ID, OperationWithState] = Map()
                  )

  private class Backend($: BackendScope[Unit, State]) {


    def render(state: State): TagOf[html.Div] = {
      <.div(

      )

    }

    def onUnmount() = Callback{
      println("StateHandlerWidget Unmouting")
    }
  }

  private val stateHandlerComponent = ScalaComponent.builder[Unit]("StateHandlerWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply = spgui.SPWidget(spwb => stateHandlerComponent())
}