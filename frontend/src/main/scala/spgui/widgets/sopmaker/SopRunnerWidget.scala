package spgui.widgets.sopmaker

import java.util.UUID
import japgolly.scalajs.react._

import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.all.svg

import spgui.communication._
import sp.domain._
import sp.domain.Logic._
import scalacss.ScalaCssReact._
import scala.scalajs.js
import spgui.components.SPWidgetElements

import diode.react.{ModelProxy, ReactConnectProxy}
import spgui.circuit._

import spgui.circuit.{ SPGUICircuit, UpdateGlobalState, GlobalState }
import spgui.{SPWidget, SPWidgetBase}
import spgui.components.Icon
import spgui.communication._
import sp.runners.APIOperationRunner
import sp.runners.APIOperationRunner.Setup
import spgui.availablemodelscircuit._

object SopRunnerWidget {

  case class State(
    sops: List[SOP] = List(),
    modelOps: List[Operation] = List(),
    opStates: Map[ID, SPValue] = Map()
  )
  case class Props(proxy: ModelProxy[ModelsCircuitState])
 
  private class Backend($: BackendScope[Props, State]) {
    val operationRunnerHandler =
      BackendCommunication.getMessageObserver(onOperationRunnerMessage, APIOperationRunner.topicResponse)

    def onOperationRunnerMessage(mess: SPMessage) = 
      mess.getBodyAs[APIOperationRunner.Response].map {
        case APIOperationRunner.Runners(setups) => {
          $.modState(_.copy(opStates = setups.head.initialState)).runNow()
        }
        case APIOperationRunner.StateEvent(
          runnerID, newRunnerStateMap, runInAuto, disableConditionGroups) => {
          $.modState(s => s.copy(opStates = s.opStates ++ newRunnerStateMap)).runNow()
        }
        case _ => Unit
      }

    def onReceiveProps(props: Props) = {
      $.modState(state => {
        props.proxy.value.activeModel.map{ model =>
          val sops = model.items.collect {
            case SOPSpec(name, sop, attributes, id) => sop
          }.flatten
          val ops = model.items.collect{
            case o:Operation => o
          }
          state.copy(
            sops = sops,
            modelOps = ops
          )
        }.getOrElse(state)
      })
    }

    def render(props: Props, state: State) = {
      <.div(
        state.sops.map{ sop => 
          SopVisualiser(sop, state.modelOps, state.opStates)
        }.toTagMod
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("SopRunnerWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillReceiveProps{
      scope => scope.backend.onReceiveProps(scope.nextProps)
    }
    .build

  val connectCircuit: ReactConnectProxy[ModelsCircuitState] = ModelsCircuit.connect(state => state)

  def apply() = spgui.SPWidget(_ => connectCircuit { proxy => component(Props(proxy)) })
}
