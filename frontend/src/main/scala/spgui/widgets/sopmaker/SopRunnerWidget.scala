package spgui.widgets.sopmaker

import java.util.UUID
import japgolly.scalajs.react._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.all.svg

import spgui.communication._
import sp.domain._
import scalacss.ScalaCssReact._
import scala.scalajs.js
import spgui.components.SPWidgetElements


import spgui.dragging._
import spgui.circuit._

import spgui.circuit.{ SPGUICircuit, UpdateGlobalState, GlobalState }
import spgui.{SPWidget, SPWidgetBase}
import spgui.components.Icon
object SopRunnerWidget {

  case class State(
    sop: SOP,
    modelIdables: List[IDAble] = List(),
    opStates: Map[ID, SPValue] = Map()
  )
 
  private class Backend($: BackendScope[Unit, State]) {
    def render(state: State) = {
      <.div(
        SopVisualiser(state.sop, ExampleSops.ops)
      )
    }
  }
  private val component = ScalaComponent.builder[Unit]("SopMakerWidget")
    .initialState(State(sop = ExampleSops.megaSop))
    .renderBackend[Backend]
    .build

  def apply() = spgui.SPWidget(spwb => component())
}
