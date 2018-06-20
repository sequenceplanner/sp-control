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

import spgui.circuit._

import spgui.circuit.{ SPGUICircuit, UpdateGlobalState, GlobalState }
import spgui.{SPWidget, SPWidgetBase}
import spgui.components.Icon
import sp.models.APIModel
import spgui.communication._
import sp.runners.APIOperationRunner
import sp.runners.APIOperationRunner.Setup

object SopRunnerWidget {

  case class State(
    sops: List[SOP] = List(),
    modelOps: List[Operation] = List(),
    opStates: Map[ID, SPValue] = Map()
  )
 
  private class Backend($: BackendScope[Unit, State]) {
    val modelMessObs = BackendCommunication.getMessageObserver(onModelObsMes, APIModel.topicResponse)
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

    def onModelObsMes(mess: SPMessage): Unit = {
      
      mess.body.to[APIModel.Response].map{
        case APIModel.SPItems(items) => {
          val sops = items.collect{
            case SOPSpec(name, sop, attributes, id) => sop
          }.flatten
          val ops = items.collect{
            case o:Operation => o
          }
          $.modState(_.copy(sops = sops, modelOps = ops)).runNow()
        }
        case x =>
      }
    }

    def render(state: State) = {
      <.div(
        state.sops.map{ sop => 
          SopVisualiser(sop, state.modelOps, state.opStates)
        }.toTagMod
      )
    }
  }
  private val component = ScalaComponent.builder[Unit]("SopMakerWidget")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply() = spgui.SPWidget(spwb => component())
}
