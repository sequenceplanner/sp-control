package spgui.widgets.VDGUI

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import rx.Obs
import sp.devicehandler.VD

import sendMessages._

import spgui.SPWidget
import spgui.components.SPWidgetElements
import spgui.communication._
import sp.domain.SPMessage
import sp.domain.Logic._
import sp.domain.SPValue
import sp.domain._
import sp.models.APIModel
import spgui.widgets.itemexplorerincontrol.ModelChoiceDropdown
import sp.vdtesting.APIVDTracker
import spgui.availablemodelscircuit.{VDCircuit, ModelsCircuitState, SetActiveModel}
import spgui.widgets.virtcom.Style

object VDTracker {
  import sp.devicehandler.APIDeviceDriver
  import sp.runners.APIOperationRunner
  import sp.abilityhandler.APIAbilityHandler
  import sp.devicehandler.APIVirtualDevice

  case class State(latestRunnerState: Map[ID, Map[ID, SPValue]] = Map(),
    latestActiveRunner: Option[ID] = None,
    latestAbilityState: Map[ID, SPValue] = Map(),
    latestVDeviceState: Map[ID, SPValue] = Map(),
    availableVDModels: List[String] = List(),
    drivers : List[VD.Driver] = List()
  )

  case class Props(modelProxy: ModelProxy[ModelsCircuitState])

  private class Backend($: BackendScope[Props, State]) {
    private val messageObservers: List[Obs] = {
      import BackendCommunication.getMessageObserver
      val pairs = List[(SPMessage => Unit, String)](
        (onOperationRunnerMessage, APIOperationRunner.topicResponse),
        (onAbilityMessage, APIAbilityHandler.topicResponse),
        (onVirtualDeviceMessage, APIVirtualDevice.topicResponse),
        (onVirtualDeviceTrackerMessage, APIVDTracker.topicResponse),
        (onDriverMessage, APIDeviceDriver.topicResponse)
      )

      pairs.map { case (onMessage, topic) => getMessageObserver(onMessage, topic) }
    }

    private val vdtObs = BackendCommunication.getWebSocketStatusObserver(
      active => { if (active) send(APIVDTracker.getModelsInfo()) },
      APIVDTracker.topicResponse
    )


    def onDriverMessage(message: SPMessage) : Unit = {
      message.body.to[APIDeviceDriver.Response].map {
        case APIDeviceDriver.TheDrivers(drivers) =>
          $.modState(_.copy(drivers = drivers.map { case (driver, _, _) => driver })).runNow()
        case APIDeviceDriver.DriverTerminated(id) =>
          val td = $.state.runNow().drivers.find(d => d.id == id)
          if(td.nonEmpty) println("Terminated " + td.get.name)
        case x =>
      }
    }

    def onVirtualDeviceTrackerMessage(message: SPMessage): Unit = {
      message.body.to[APIVDTracker.Response].map{
        case APIVDTracker.OpRunnerCreated(id) =>
          $.modState(s => s.copy(latestActiveRunner = Some(id))).runNow()

        case APIVDTracker.sendModelsInfo(models) =>
          $.modState(s => s.copy(availableVDModels = models)).runNow()

        case x => Unit
      }
    }

    def onOperationRunnerMessage(mess: SPMessage): Unit = {
      mess.body.to[APIOperationRunner.Response].map {
        case APIOperationRunner.StateEvent(runnerID, state, _, _) =>
          $.modState { s =>
            val updatedRunnerState = s.latestRunnerState.getOrElse(runnerID, Map()) ++ state
            s.copy(latestRunnerState = s.latestRunnerState ++ Map(runnerID -> updatedRunnerState))
          }.runNow()

        case _ => Unit
      }
    }

    def onAbilityMessage(mess: SPMessage): Unit = {
      mess.body.to[APIAbilityHandler.Response].map{
        case APIAbilityHandler.AbilityState(_, state) =>
          $.modState(s => s.copy(latestAbilityState = s.latestAbilityState ++ state)).runNow()

        case APIAbilityHandler.AbilitiesTerminated =>
          println("Abilities terminated")

        case _ => Unit
      }
    }
    def onVirtualDeviceMessage(mess: SPMessage): Unit = {
      mess.body.to[APIVirtualDevice.Response].map{
        case APIVirtualDevice.StateEvent(_, _, state, _) =>
          $.modState(s => s.copy(latestVDeviceState = s.latestVDeviceState ++ state)).runNow()

        case APIVirtualDevice.TerminatedAllVDs =>
          println("VDs terminated")

        case _ => Unit
      }
    }

    def send(request: APIVDTracker.Request) = Callback {
      CommunicationAPI.Communicator.Device.postRequest(request)
    }

    def send(modelId: ID, request: APIModel.Request, from: String = "VDTrackerWidget"): Callback = Callback {
      CommunicationAPI.Communicator.Model.postRequest(modelId, request, from)
    }

    def send(request: APIOperationRunner.Request): Callback = Callback {
      CommunicationAPI.Communicator.OperationRunner.postRequest(request)
    }

    def render(props: Props, state: State) = {
      import sp.models.APIModel

      def onModelClick(modelName: String): Callback = send(APIVDTracker.createModel(modelName))
      def onModelChoiceClick(modelId: ID): Callback = {
        props.modelProxy.dispatchCB(SetActiveModel(modelId)) >> send(modelId, APIModel.GetItemList(0, 99999))
      }

      def launchAbilities: Callback = {
        Callback {
          props.modelProxy.value.activeModel.foreach { model =>
            CommunicationAPI.Communicator.Device.postRequest(APIVDTracker.launchVDAbilities(model.items))
          }
        }
      }

      val models = state.availableVDModels.map { model => SPWidgetElements.dropdownElement(model, onModelClick(model)) }
      val idAbles = props.modelProxy.value.activeModel.map(_.items).getOrElse(List())

      <.div(
        SPWidgetElements.buttonGroup(Seq(
          SPWidgetElements.dropdown("Create Model", models),
          ModelChoiceDropdown(onModelChoiceClick),
          SPWidgetElements.button("Launch VD and Abilities", launchAbilities),
          SPWidgetElements.button("Launch operation runner", send(APIVDTracker.launchOpRunner(idAbles))),
          SPWidgetElements.button("Terminate Everything", terminateAll(state))
        )),
        <.br(),
        renderRunners(state.latestActiveRunner, state.latestRunnerState, idAbles),
        <.br(),
        renderInfo("Ability state", state.latestAbilityState, idAbles),
        <.br(),
        renderInfo("VDevice state", state.latestVDeviceState, idAbles)
      )
    }

    def terminateAll(s: State): Callback = Callback {
      terminateAbilities()
      terminateDrivers(s.drivers) // Todo: also remove all drivers from gui disp? */
      terminateVDs()

      println("Terminating runners..")
      terminateRunners(s.latestRunnerState).runNow()
      // Todo: terminate virtualDevice
    }

    def terminateVDs(): Unit = {
      CommunicationAPI.Communicator.Device.postRequest(APIVirtualDevice.TerminateAllVDs)
    }

    def terminateAbilities(): Unit = {
      CommunicationAPI.Communicator.AbilityHandler.postRequest(APIAbilityHandler.TerminateAllAbilities)
    }

    def terminateDrivers(drivers : List[VD.Driver]): Unit = {
      drivers.map(_.id).foreach { id =>
        CommunicationAPI.Communicator.Device.postRequest(APIDeviceDriver.TerminateDriver(id))
      }
    }

    def terminateRunners(runners: Map[ID, Map[ID,SPValue]]): Callback = {
      runners.keys.foreach { runnerID =>
        CommunicationAPI.Communicator.OperationRunner.postRequest(APIOperationRunner.TerminateRunner(runnerID))
      }

      // todo: should wait for new state run runner service...
      $.modState(_.copy(latestRunnerState = Map()))
    }

    def terminateRunner(id: ID): Callback = {
      CommunicationAPI.Communicator.OperationRunner.postRequest(APIOperationRunner.TerminateRunner(id))

      // todo: should wait for new state run runner service...
      $.modState(state => state.copy(latestRunnerState = state.latestRunnerState - id))
    }

    def renderRunners(active: Option[ID], runnerStates: Map[ID, Map[ID , SPValue]], ids : List[IDAble]): TagMod = {
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

    def renderInfo(name : String, m: Map[ID , SPValue], ids : List[IDAble]): TagMod = {
      val state = (for {
        (id,spval) <- m
        thing <- ids.find(_.id==id)
      } yield {
        thing -> spval
      }).toList.sortBy(tv => tv._1.name)
      <.details(^.open := "open", ^.className := Style.collapsible.htmlClass,
        <.summary(name),
        <.table(
          ^.className := "table table-striped", ^.className := "Table",
          <.tbody(
            state.map { case (thing, value) =>
              <.tr(
                <.td(thing.name),
                <.td(thing.id.toString),
                <.td(value.toString)
              )
            }.toTagMod
          )), <.br()
      ).when(m.nonEmpty)
    }

    def onMount(): Callback = {
      sendToVDTrackerService(APIVDTracker.getModelsInfo())
    }


    def onUnMount(): Callback = Callback {
      messageObservers.foreach(_.kill())
      vdtObs.kill()
    }
  }

  private val component = ScalaComponent.builder[Props]("VDTracker")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount())
    .componentWillUnmount(_.backend.onUnMount())
    .build

  val connectCircuit: ReactConnectProxy[ModelsCircuitState] = VDCircuit.connectModels

  def apply() = SPWidget(_ => connectCircuit { proxy =>  component(Props(proxy)) })
}
