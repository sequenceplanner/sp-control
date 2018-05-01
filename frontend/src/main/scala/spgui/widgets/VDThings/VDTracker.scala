package spgui.widgets.vdtesting

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.VD
import sp.devicehandler.VD.OneToOneMapper
import spgui.circuit.{SPGUICircuit, SetTheme}
import spgui.SPWidget
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
import sp.unification.APIUnification
import spgui.widgets.virtcom.Style
import spgui.widgets.virtcom.dropdownWithScroll.dropdownWScroll

object VDTracker {
  import sp.runners.APIOperationRunner
  import sp.abilityhandler.APIAbilityHandler
  import sp.devicehandler.APIVirtualDevice


  case class State(modelNames : Set[String] = Set(), // names of models that can be created
                   latestEvent: Map[ID, SPValue] = Map(),
                   latestAbilityState: Map[ID, SPValue] = Map(),
                   latestVDeviceState: Map[ID, SPValue] = Map(),
                   modelIdables : List[IDAble] = List(), // Created model idables
                   modelID : ID = ID.newID // Created model id
                  )

  private class Backend($: BackendScope[Unit, State]) {

    val unificationHandler =
      BackendCommunication.getMessageObserver(onUnificationMessage, APIUnification.topicResponse)
    val operationRunnerHandler =
      BackendCommunication.getMessageObserver(onOperationRunnerMessage, APIOperationRunner.topicResponse)
    val abilityHandler =
      BackendCommunication.getMessageObserver(onAbilityMessage, APIAbilityHandler.topicResponse)
    val virtualDeviceHandler =
      BackendCommunication.getMessageObserver(onVirtualDeviceMessage, APIVirtualDevice.topicResponse)
    val modelMessObs =
      BackendCommunication.getMessageObserver(onModelObsMes, mapi.topicResponse)

    def onUnificationMessage(mess: SPMessage) : Unit = {
      mess.body.to[APIUnification.Response].map {
        case APIUnification.sendUnificationModelInfo(modelName, modelTags) => {
          $.modState(s => s.copy(modelNames = (s.modelNames + modelName))).runNow()
        }
      }
    }

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
    def send(mess: APIUnification.Request): Callback = {
      val h = SPHeader(from = "VDTrackerWidget", to = APIUnification.service, reply = SPValue("VDTracker"))
      val json = SPMessage.make(h, mess) // *(...) is a shorthand for toSpValue(...)
      BackendCommunication.publish(json, APIUnification.topicRequest)
      Callback.empty
    }

    def render(p:Unit, s:State) =
      <.div(
        <.div(^.onClick --> send(APIUnification.getUnificationModelsInfo()),
          renderCreateableModels(s))
        ,
        ModelChoiceDropdown(id => {$.modState(_.copy(modelID = id)); sendToModel(id, mapi.GetItemList(0,99999))}), // Get all models in dropdown
        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> send(APIUnification.launchVDAbilities(s.modelIdables)), "Launch VD and Abilities"
        ),
        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> send(APIUnification.launchOpRunner(s.modelIdables)), "Launch operation runner"
        ),
        <.br(),
        renderInfo("Operation runner variables", s.latestEvent, s.modelIdables),
        <.br(),
        renderInfo("Ability state", s.latestAbilityState, s.modelIdables),
        <.br(),
        renderInfo("VDevice state", s.latestVDeviceState, s.modelIdables),
      )

      def renderCreateableModels(s:State) = {
        <.div( // find idables with robot schedule and put them in a dropdown menu
          dropdownWScroll("Create model", s.modelNames.toList.map(m => <.div(m, ^.onClick --> {
            send(APIUnification.getUnificationModel(m)); $.modState(_.copy(modelID = s.modelID))
          }, ^.className := Style.schSelect.htmlClass))))
      }

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
      unificationHandler.kill()
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
