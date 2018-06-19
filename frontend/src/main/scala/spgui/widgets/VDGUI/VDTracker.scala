package spgui.widgets.VDGUI

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.VD
import sp.devicehandler.VD.OneToOneMapper
import spgui.circuit.{SPGUICircuit, SetTheme}
import spgui.SPWidget
import spgui.components.Icon
import spgui.components.SPWidgetElements
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
import spgui.widgets.VDGUI.sendMessages.sendToDeviceDriver
import spgui.widgets.virtcom.Style

object VDTracker {
  //import sp.devicehandler._
  import sp.devicehandler.APIDeviceDriver
  import sp.runners.APIOperationRunner
  import sp.abilityhandler.APIAbilityHandler
  import sp.devicehandler.APIVirtualDevice
  import spgui.widgets.virtcom.dropdownWithScroll.dropdownWScroll

  case class State(latestRunnerState: Map[ID, Map[ID, SPValue]] = Map(),
    latestActiveRunner: Option[ID] = None,
    latestAbilityState: Map[ID, SPValue] = Map(),
    latestVDeviceState: Map[ID, SPValue] = Map(),
    availableVDModels: List[String] = List(),
    modelIdables : List[IDAble] = List(),
    modelID : ID = ID.newID,
    drivers : List[VD.Driver] = List()
  )


  private class Backend($: BackendScope[Unit, State]) {



    val operationRunnerHandler = BackendCommunication.getMessageObserver(onOperationRunnerMessage, APIOperationRunner.topicResponse)
    val abilityHandler =   BackendCommunication.getMessageObserver(onAbilityMessage, APIAbilityHandler.topicResponse)
    val virtualDeviceHandler =  BackendCommunication.getMessageObserver(onVirtualDeviceMessage, APIVirtualDevice.topicResponse)
    val modelMessObs =  BackendCommunication.getMessageObserver(onModelObsMes, mapi.topicResponse)
    val vdModelObs =    BackendCommunication.getMessageObserver(onVDTmsg, APIVDTracker.topicResponse)
    val driverHandler = BackendCommunication.getMessageObserver(onDriverMessage, APIDeviceDriver.topicResponse)

    val vdtObs = BackendCommunication.getWebSocketStatusObserver(mess => {
      if (mess) send(APIVDTracker.getModelsInfo())
    }, APIVDTracker.topicResponse)


    def onDriverMessage(mess: SPMessage) : Unit = {
      mess.body.to[APIDeviceDriver.Response].map {
        case APIDeviceDriver.TheDrivers(ds) =>
          $.modState ( _.copy(drivers = ds.map(_._1) ) ).runNow()
        case APIDeviceDriver.DriverTerminated(id) =>
          val td = $.state.runNow().drivers.find(d => d.id == id)
          if(td.nonEmpty) println("Terminated " + td.get.name)
        case x =>
      }
    }

    def onVDTmsg(mess: SPMessage): Unit = {
      mess.body.to[APIVDTracker.Response].map{
        case APIVDTracker.OpRunnerCreated(id) => {
          $.modState(s => s.copy(latestActiveRunner = Some(id))).runNow()
        }
        case APIVDTracker.sendModelsInfo(models) => {
          $.modState(s => s.copy(availableVDModels = models)).runNow()
        }
        case x =>
      }
    }

    def onOperationRunnerMessage(mess: SPMessage): Unit = {
      mess.body.to[APIOperationRunner.Response].map{
        case APIOperationRunner.StateEvent(runnerID, state, auto, groups) => {
          $.modState{s =>
            val updRs = s.latestRunnerState.get(runnerID).getOrElse(Map()) ++ state
            s.copy(latestRunnerState = s.latestRunnerState ++ Map(runnerID -> updRs))
          }.runNow()
        }
        case x =>
      }
    }

    def onAbilityMessage(mess: SPMessage): Unit = {
      mess.body.to[APIAbilityHandler.Response].map{
        case APIAbilityHandler.AbilityState(id, state) => {
          $.modState(s => s.copy(latestAbilityState = s.latestAbilityState ++ state)).runNow()
        }
        case APIAbilityHandler.AbilitiesTerminated =>
          println("Abilities terminated")
        case x =>
      }
    }
    def onVirtualDeviceMessage(mess: SPMessage): Unit = {
      mess.body.to[APIVirtualDevice.Response].map{
        case APIVirtualDevice.StateEvent(resource, id, state, diff) => {
          $.modState(s => s.copy(latestVDeviceState = s.latestVDeviceState ++ state)).runNow()
        }
        case x =>
      }
    }
    def onModelObsMes(mess: SPMessage): Unit = {
      mess.body.to[mapi.Response].map{
        case mapi.SPItems(items) => {
          $.modState(_.copy(modelIdables = items)).runNow()
        }
        case x =>
      }
    }


    def sendToModel(model: ID, mess: mapi.Request)= Callback{ //  Send message to model
      val h = SPHeader(from = "VDTrackerWidget", to = model.toString,
        reply = SPValue("VDTrackerWidget"))
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, mapi.topicRequest)
    }

    def send(mess: APIVDTracker.Request) =  Callback{
      val h = SPHeader(from = "VDTrackerWidget", to = APIVDTracker.service, reply = SPValue("VDTracker"))
      val json = SPMessage.make(h, mess) // *(...) is a shorthand for toSpValue(...)
        BackendCommunication.publish(json, APIVDTracker.topicRequest)
    }

    def render(p:Unit, s:State) =
      <.div(
        SPWidgetElements.buttonGroup(Seq(
          SPWidgetElements.dropdown(
            "Create Model",
            s.availableVDModels.map(m =>
              SPWidgetElements.dropdownElement(
                m,
                {
                  send(APIVDTracker.createModel(m)) >>
                  $.modState(_.copy(modelID = s.modelID))
                }
              )
            ).toSeq
          ),
          ModelChoiceDropdown(id => {
            $.modState(_.copy(modelID = id)) >>
            sendToModel(id, mapi.GetItemList(0,99999))
          }),
          SPWidgetElements.button(
            "Launch VD and Abilities",
            send(APIVDTracker.launchVDAbilities(s.modelIdables))
          ),
          SPWidgetElements.button(
            "Launch operation runner",
            send(APIVDTracker.launchOpRunner(s.modelIdables))
          ),
          SPWidgetElements.button(
            "Terminate Everything",
            terminateAll(s)
          )
        )),
        <.br(),
        renderRunners(s.latestActiveRunner, s.latestRunnerState, s.modelIdables),
        <.br(),
        renderInfo("Ability state", s.latestAbilityState, s.modelIdables),
        <.br(),
        renderInfo("VDevice state", s.latestVDeviceState, s.modelIdables)
      )

    def terminateAll(s: State) = Callback{
      terminateAbilities
      terminateDrivers(s.drivers) // Todo: also remove all drivers from gui disp? */
      println("Terminating runners..")
      terminateRunners(s.latestRunnerState).runNow()
      // Todo: terminate virtualDevice
    }

    def terminateAbilities = {
      val h = SPHeader(from = "VDTrackerWidget", to = APIAbilityHandler.service)
      val json = SPMessage.make(h, APIAbilityHandler.TerminateAllAbilities)
      BackendCommunication.publish(json, APIAbilityHandler.topicRequest)
    }

    def terminateDrivers(drivers : List[VD.Driver]) = {
      val h = SPHeader(from = "VDTrackerWidget", to = "DriverService")
      drivers.map(d => { val json = SPMessage.make(h, APIDeviceDriver.TerminateDriver(d.id))
        BackendCommunication.publish(json, APIDeviceDriver.topicRequest) })
    }

    def terminateRunners(rs : Map[ID, Map[ID,SPValue]]): Callback = {
      val h = SPHeader(from = "VDTrackerWidget", to = APIOperationRunner.service)
      rs.keys.map(r => { val json = SPMessage.make(h, APIOperationRunner.TerminateRunner(r))
        BackendCommunication.publish(json, APIOperationRunner.topicRequest) })

      // todo: should wait for new state run runner service...
      $.modState(s=>s.copy(latestRunnerState = Map()))
    }

    def terminateRunner(id: ID): Callback = {
      val h = SPHeader(from = "VDTrackerWidget", to = APIOperationRunner.service)
      val json = SPMessage.make(h, APIOperationRunner.TerminateRunner(id))
      BackendCommunication.publish(json, APIOperationRunner.topicRequest)
      // todo: should wait for new state run runner service...
      $.modState(s=>s.copy(latestRunnerState = s.latestRunnerState - id))
    }

    def renderRunners(active: Option[ID], runnerStates: Map[ID, Map[ID , SPValue]], ids : List[IDAble]) = {
      runnerStates.map { case (r, state) =>
        <.details(if (active.contains(r)) ^.open := "open" else EmptyVdom, ^.className := Style.collapsible.htmlClass,
          <.summary(
            s"Operation runner state ($r)",
            <.button(
              ^.className := "btn",
              ^.title := "Kill runner",
              ^.onClick --> terminateRunner(r),
              <.i(^.className := "fa fa-bolt")
            )
          ),
          <.table(
            ^.className := "table table-striped", ^.className := "Table",
            <.tbody(
              state.map(is => {
                val cI = ids.filter(i => i.id == is._1)
                <.tr(
                  <.td(if (cI.nonEmpty) cI.head.name else ""),
                  <.td(is._1.toString),
                  <.td(is._2.toString)
                )
              }).toTagMod
            )), <.br()
        ).when(state.nonEmpty)
      }.toTagMod.when(runnerStates.nonEmpty)
    }

    def renderInfo(name : String, m: Map[ID , SPValue], ids : List[IDAble]) = {
      val state = (for {
        (id,spval) <- m
        thing <- ids.find(_.id==id)
      } yield {
        (thing -> spval)
      }).toList.sortBy(tv => tv._1.name)
      <.details(^.open := "open", ^.className := Style.collapsible.htmlClass,
        <.summary(name),
        <.table(
          ^.className := "table table-striped", ^.className := "Table",
          <.tbody(
            state.map { case (t,v) => {
              <.tr(
                <.td(t.name),
                <.td(t.id.toString),
                <.td(v.toString)
              )
            }}.toTagMod
          )), <.br()
      ).when(m.nonEmpty)
    }

    def onMount(): Callback = {
      send(APIVDTracker.getModelsInfo())
    }

    def onUnmount() =  Callback{
      operationRunnerHandler.kill()
      abilityHandler.kill()
      virtualDeviceHandler.kill()
      modelMessObs.kill()
      vdModelObs.kill()
      vdtObs.kill()
    }
  }

  private val component = ScalaComponent.builder[Unit]("VDTracker")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount())
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = SPWidget(swpb => component())
}
