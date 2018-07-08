package spgui.widgets.VDGUI

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.VD
import spgui.circuit.{SPGUICircuit, SetTheme}
import spgui.SPWidget
import spgui.components.Icon
import spgui.components.SPWidgetElements
import spgui.communication._
import sp.domain.Logic._
import sp.models.{APIModel => mapi}
import sendMessages._

import sp.domain._
import spgui.widgets.itemexplorerincontrol.ModelChoiceDropdown
import sp.vdtesting.APIVDTracker
import spgui.widgets.virtcom.Style

import sp.VDAggregator.APIVDAggregator

object VDTracker {
  import sp.runners.APIOperationRunner
  import sp.abilityhandler.APIAbilityHandler
  import sp.devicehandler.APIVirtualDevice

  case class State(latestRunnerState: Map[ID, Map[ID, SPValue]] = Map(),
    latestActiveRunner: Option[ID] = None,
    latestAbilityState: Map[ID, SPValue] = Map(),
    latestVDeviceState: Map[ID, SPValue] = Map(),
    availableVDModels: List[String] = List(),
    modelIdables : List[IDAble] = List(),
    modelID : Option[ID] = None,
    drivers : List[VD.Driver] = List()
  )


  private class Backend($: BackendScope[Unit, State]) {

    val modelMessObs =  BackendCommunication.getMessageObserver(onModelObsMes, mapi.topicResponse)
    val vdModelObs =    BackendCommunication.getMessageObserver(onVDTmsg, APIVDTracker.topicResponse)

    val aggregatorHandler = BackendCommunication.getMessageObserver(onAggregatorMessage, APIVDAggregator.topicResponse)

    val vdtObs = BackendCommunication.getWebSocketStatusObserver(mess => {
      if (mess) sendToVDTrackerService(APIVDTracker.getModelsInfo())
    }, APIVDTracker.topicResponse)


    def onAggregatorMessage(mess: SPMessage) : Unit = {
      mess.body.to[APIVDAggregator.Response].map {
        case APIVDAggregator.TheDrivers(ds) =>
          $.modState ( _.copy(drivers = ds.map(_.driver) ) ).runNow()

        case APIVDAggregator.TheAbilityStates(abilityStates) =>
          $.modState(s => s.copy(latestAbilityState = abilityStates)).runNow()
        case APIVDAggregator.TheResources(resources) =>
          $.modState(s => s.copy(latestVDeviceState = resources.map(_.state.seq).reduce(_ ++ _)) ).runNow()
        case APIVDAggregator.TheRunnerStates(runnerStates) =>
          $.modState{_.copy(latestRunnerState = runnerStates )}.runNow()
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


    def onModelObsMes(mess: SPMessage): Unit = {
      mess.body.to[mapi.Response].map{
        case mapi.SPItems(items) => {
          $.modState(_.copy(modelIdables = items)).runNow()
        }
        case x =>
      }
    }


    def render(p:Unit, s:State) =
      <.div(
        SPWidgetElements.button(
          "reload data",
          sendToVDTrackerService(APIVDTracker.getModelsInfo())
        ),
         <.p(s.latestRunnerState.toString()),
        SPWidgetElements.buttonGroup(Seq(
          SPWidgetElements.dropdown(
            "Create Model",
            s.availableVDModels.map(m =>
              SPWidgetElements.dropdownElement(
                m,
                {
                  sendToVDTrackerService(APIVDTracker.createModel(m)) >>
                  $.modState(_.copy(modelID = s.modelID))
                }
              )
            ).toSeq
          ),
          ModelChoiceDropdown(id => {
            $.modState(_.copy(modelID = Some(id))) >>
            sendToModel(id, mapi.GetItemList(0,99999))
          }),
         s.modelID.whenDefined{id => TagMod(
            SPWidgetElements.button(
              s"Launch VD and Abilities",
              sendToVDTrackerService(APIVDTracker.launchVDAbilities(s.modelIdables))
            ),
            SPWidgetElements.button(
              "Launch operation runner",
              sendToVDTrackerService(APIVDTracker.launchOpRunner(s.modelIdables))
            ),
            SPWidgetElements.button(
              "Terminate Everything",
              terminateAll(s)
            )


         )},
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
      terminateDrivers(s.drivers)
      terminateVDs
      println("Terminating runners..")
      terminateRunners(s.latestRunnerState)
      resetVDGUI(s) // todo: should wait for responses from backend
      $.modState(s=>s.copy(latestRunnerState = Map(), latestAbilityState = Map(), latestVDeviceState = Map())).runNow()
    }

    def terminateRunner(id: ID): Callback = {
      sendTerminateRunner(id)
      // todo: should wait for new state run runner service...
      $.modState(s=>s.copy(latestRunnerState = s.latestRunnerState - id))
    }

    def createCorrectTypeOfSPValue(sPValue: SPValue, newValue : String) : SPValue =  { // Convert the incoming string to an SPvalue of the same type as the previous state value
      if (sPValue.validate[Int].isSuccess)          {SPValue(newValue.toInt)}
      else if(sPValue.validate[Boolean].isSuccess)  {SPValue(newValue.toBoolean)}
      else                                          {SPValue(newValue)}
    }



    def updateDriverState(runner: ID, state: Map[ID , SPValue], key: ID)(e: ReactKeyboardEventFromInput) = {
      if(e.key == "Enter") {
        val newState = state + (key -> createCorrectTypeOfSPValue(state(key), e.target.value))
        sendToRunner(APIOperationRunner.SetState(runner, newState) )
      }
      else
        Callback.empty
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
                val cI = ids.find(i => i.id == is._1)
                <.tr(
                  cI.whenDefined(x => <.td(x.name)),
                  <.td(is._1.toString),
                  <.td(is._2.toString),
                  <.td(<.input(^.placeholder := "Change value...", ^.onKeyPress ==> { updateDriverState(r, state, is._1)}, ^.className := DriverWidgetCSS.input.htmlClass))
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
      sendToVDTrackerService(APIVDTracker.getModelsInfo())
    }

    def onUnmount() =  Callback{
      modelMessObs.kill()
      vdModelObs.kill()
      aggregatorHandler.kill()
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
