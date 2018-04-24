package spgui.widgets.vdtesting

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.VD
import sp.devicehandler.VD.OneToOneMapper
import spgui.circuit.{SPGUICircuit, SetTheme}
import spgui.SPWidget
import spgui.components.Icon
import spgui.components.{SPWidgetElements => Comp}
import spgui.communication._
import sp.domain.SPAttributes._
import sp.domain.SPMessage
import sp.domain.Logic._
import sp.domain.SPValue

import scalajs.js._
import sp.domain._
import spgui.widgets.itemexplorerincontrol.ModelChoiceDropdown
import sp.models.{APIModel => mapi}
import sp.vdtesting.APIVDTracker
import spgui.widgets.virtcom.Style

object VDTracker {
  //import sp.devicehandler._
  import sp.devicehandler.APIDeviceDriver
  import sp.runners.APIOperationRunner
  import sp.abilityhandler.APIAbilityHandler
  import sp.devicehandler.APIVirtualDevice

  case class State(latestEvent: Map[ID, SPValue] = Map(),
                   latestAbilityState: Map[ID, SPValue] = Map(),
                   latestVDeviceState: Map[ID, SPValue] = Map(),
                   modelIdables : List[IDAble] = List(),
                   modelID : ID = ID.newID
                  )


  private class Backend($: BackendScope[Unit, State]) {
    
    val operationRunnerHandler =
      BackendCommunication.getMessageObserver(onOperationRunnerMessage, APIOperationRunner.topicResponse)
    val abilityHandler =
      BackendCommunication.getMessageObserver(onAbilityMessage, APIAbilityHandler.topicResponse)
    val virtualDeviceHandler =
      BackendCommunication.getMessageObserver(onVirtualDeviceMessage, APIVirtualDevice.topicResponse)
    val modelMessObs =
      BackendCommunication.getMessageObserver(onModelObsMes, mapi.topicResponse)


    def onOperationRunnerMessage(mess: SPMessage): Unit = {
      mess.body.to[APIOperationRunner.Response].map{
        case APIOperationRunner.StateEvent(runnerID, state) => {
          $.modState(s => s.copy(latestEvent = state)).runNow()
        }
      }

    }
    def onAbilityMessage(mess: SPMessage): Unit = {
      mess.body.to[APIAbilityHandler.Response].map{
        case APIAbilityHandler.AbilityState(id, state) => {
          $.modState(s => s.copy(latestAbilityState = state)).runNow()
        }
      }
    }
    def onVirtualDeviceMessage(mess: SPMessage): Unit = {
      mess.body.to[APIVirtualDevice.Response].map{
        case APIVirtualDevice.StateEvent(resource, id, state, diff) => {
          $.modState(s => s.copy(latestVDeviceState = state)).runNow()
        }
      }
    }
    def onModelObsMes(mess: SPMessage): Unit = {
      mess.body.to[mapi.Response].map{
        case mapi.SPItems(items) => {
          $.modState(_.copy(modelIdables = items)).runNow()
        }
      }
    }


    def sendToModel(model: ID, mess: mapi.Request): Callback = { //  Send message to model
      val h = SPHeader(from = "VolvoSchedulerWidget", to = model.toString,
        reply = SPValue("VolvoSchedulerWidget"))
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, mapi.topicRequest)
      Callback.empty
    }
    def send(mess: APIVDTracker.Request): Callback = {
      val h = SPHeader(from = "VDTrackerWidget", to = APIVDTracker.service, reply = SPValue("VDTracker"))
      val json = SPMessage.make(h, mess) // *(...) is a shorthand for toSpValue(...)
      BackendCommunication.publish(json, APIVDTracker.topicRequest)
      Callback.empty
    }

    def render(p:Unit, s:State) =
      <.div(
        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> send(APIVDTracker.createModel()), "Create model"
        ),
        ModelChoiceDropdown(id => {$.modState(_.copy(modelID = id)); sendToModel(id, mapi.GetItemList(0,99999))}), // Get all models in dropdown
        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> send(APIVDTracker.launchVDAbilities(s.modelIdables)), "Launch VD and Abilities"
        ),
        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> send(APIVDTracker.launchOpRunner(s.modelIdables)), "Launch operation runner"
        ),
        <.br(),
        renderInfo("Operation runner variables", s.latestEvent, s.modelIdables),
        <.br(),
        renderInfo("Ability state", s.latestAbilityState, s.modelIdables),
        <.br(),
        renderInfo("VDevice state", s.latestVDeviceState, s.modelIdables),
      )


    def renderInfo(name : String, m: Map[ID , SPValue], ids : List[IDAble]) = {
      <.details(^.open := "open", ^.className := Style.collapsible.htmlClass,
        <.summary(name),
        <.table(
          ^.className := "table table-striped", ^.className := "Table",
          <.tbody(
            m.map(is => {
              val cI = ids.filter(i => i.id == is._1)
              <.tr(
                <.td(if (cI.nonEmpty) cI.head.name else ""),
                <.td(is._1.toString),
                <.td(is._2.toString)
              )
            }).toTagMod
          )), <.br()
      ).when(m.nonEmpty)
    }

    def onUnmount(): Callback =  {
      operationRunnerHandler.kill()
      abilityHandler.kill()
      virtualDeviceHandler.kill()
      modelMessObs.kill()
      Callback.empty
    }
  }

  private val component = ScalaComponent.builder[Unit]("VDTracker")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = SPWidget(swpb => component())
}
