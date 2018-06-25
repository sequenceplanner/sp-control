package spgui.widgets.sopmaker

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._

import spgui.components.SPWidgetElements
import diode.react.{ModelProxy, ReactConnectProxy}
import spgui.communication._
import sp.runners.APIOperationRunner
import spgui.circuits.main.MainCircuit
import spgui.circuits.main.handlers.ModelsCircuitState

object SopRunnerWidget {

  case class State(
    sopSpecs: List[SOPSpec] = List(),
    modelOps: List[Operation] = List(),
    opStates: Map[ID, SPValue] = Map(),
    currentSop: Option[SOP] = None
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
          val sopSpecs = model.items.collect {
            case spec:SOPSpec => spec
          }
          val ops = model.items.collect{
            case o:Operation => o
          }
          state.copy(
            sopSpecs = sopSpecs,
            modelOps = ops
          )
        }.getOrElse(state)
      })
    }

    def setSopSpec(spec: SOPSpec) = $.modState(_.copy(
      currentSop = Some(spec.sop.head)
    ))

    def render(props: Props, state: State) = {
      <.div(
        SPWidgetElements.dropdown(
          "Choose SOP",
          state.sopSpecs.map(
            spec => SPWidgetElements.dropdownElement(spec.name, setSopSpec(spec))
          )
        ),
        state.currentSop match {
          case Some(sop) => SopVisualiser(sop, state.modelOps, state.opStates)
          case None => EmptyVdom
        }
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

  val connectCircuit: ReactConnectProxy[ModelsCircuitState] = MainCircuit.connect(_.models)

  def apply() = spgui.SPWidget(_ => connectCircuit { proxy => component(Props(proxy)) })
}
