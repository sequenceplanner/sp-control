package spgui.widgets.VDGUI

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.internal.StyleA
import sp.domain.{SPValue, _}
import sp.vdtesting.APIVDTracker
import spgui.{SPWidget, SimpleSet}
import spgui.circuits.main.handlers.Aliases._
import spgui.circuits.main.handlers.DriverHandler.DriverId
import spgui.circuits.main.{FrontendState, MainCircuit}
import spgui.circuits.main.handlers._
import spgui.communication._
import spgui.components.SPWidgetElements
import spgui.widgets.itemexplorerincontrol.ModelChoiceDropdown
import spgui.widgets.virtcom.Style

object NewVDTracker {
  import sp.abilityhandler.APIAbilityHandler
  import sp.devicehandler.{APIDeviceDriver, APIVirtualDevice}
  import sp.runners.APIOperationRunner

  case class Props(proxy: ModelProxy[FrontendState]) {
    val activeModel: Option[ModelMock] = proxy.value.models.activeModel
    val activeRunnerId: Option[RunnerId] = proxy.value.virtualDevices.latestActiveRunnerId

    def activeRunner: Option[Runner] = {
      for {
        runnerId <- proxy.value.virtualDevices.latestActiveRunnerId
        runner <- proxy.value.runners.runners.get(runnerId)
      } yield runner
    }

    def abilities: SimpleSet[AbilityId, AbilityData] = proxy.value.abilities.abilities
    def runners: SimpleSet[RunnerId, Runner] = proxy.value.runners.runners
    def drivers: SimpleSet[DriverId, DriverInfo] = proxy.value.drivers.drivers
    def models: SimpleSet[VDModelId, ModelMock] = proxy.value.models.models
    def availableVDModels: List[VDModelName] = proxy.value.virtualDevices.availableVDModels
    def virtualDevices: SimpleSet[VirtualDeviceId, VDData] = proxy.value.virtualDevices.virtualDevices
  }

  private class Backend($: BackendScope[Props, Unit]) {
    def render(props: Props) = {
      import sp.models.APIModel

      def onModelClick(modelName: String): Callback = Callback {
        VDTrackerCommunication.postRequest(APIVDTracker.createModel(modelName))
      }

      def onModelChoiceClick(modelId: ID): Callback = {
        props.proxy.dispatchCB(SetActiveModel(modelId)) >>
          Callback { ModelCommunication.postRequest(modelId, APIModel.GetItemList(0, 99999)) }
      }

      def launchAbilities: Callback = {
        Callback {
          props.activeModel.foreach { model =>
            VDTrackerCommunication.postRequest(APIVDTracker.launchVDAbilities(model.items))
          }
        }
      }

      val models = props.availableVDModels.map { model => SPWidgetElements.dropdownElement(model, onModelClick(model)) }
      val idAbles = props.activeModel.map(_.items).getOrElse(List())
      val runners = props.runners.toList
      val abilityStates = props.abilities.map(_.state).reduce(_ ++ _)
      val resourceStates = props.virtualDevices.flatMap(_.resources.toList).map(_.state).reduce(_ ++ _)

      <.div(
        SPWidgetElements.buttonGroup(Seq(
          SPWidgetElements.dropdown("Create Model", models),
          ModelChoiceDropdown(onModelChoiceClick),
          SPWidgetElements.button("Launch VD and Abilities", launchAbilities),
          SPWidgetElements.button("Launch operation runner", Callback { VDTrackerCommunication.postRequest(APIVDTracker.launchOpRunner(idAbles)) }),
          SPWidgetElements.button("Terminate Everything", terminateAll(props))
        )),
        <.br(),
        props.activeRunnerId.map(activeRunnerId => renderRunners(activeRunnerId, runners, idAbles)).whenDefined,
        <.br(),
        renderInfo("Ability state", abilityStates, idAbles),
        <.br(),
        renderInfo("Virtual Device state", resourceStates, idAbles)
      )
    }

    def terminateAll(props: Props): Callback = Callback {
      terminateAbilities()
      terminateDrivers(props.drivers.map(_.id)) // Todo: also remove all drivers from gui disp? */
      terminateVDs()

      println("Terminating runners..")
      terminateRunners(props.runners.map(_.id))
      // Todo: terminate virtualDevice
    }

    def renderRunners(activeRunnerId: RunnerId, runners: List[Runner], ids : List[IDAble]): TagMod = {
      runners.map { runner =>
        val active = runner.id == activeRunnerId
        val state = runner.operations.flatMap(_.state).toMap

        val rows = state.map { case (id, value) =>
            val name = ids.find(_.id == id).map(_.name).getOrElse("")

            <.tr(
              <.td(name),
              <.td(id.toString),
              <.td(value.toString())
            )
        }.toTagMod

        <.details(
          (^.open := "open").when(active),
          ^.className := Style.collapsible.htmlClass,
          <.summary(
            s"Operation runner state (${runner.id})",
            <.button(
              ^.className := "btn",
              ^.title := "Kill runner",
              ^.onClick --> Callback { terminateRunner(runner.id) },
              <.i(^.className := "fa fa-bolt")
            )
          ),
          <.table(^.className := "table table-striped Table", <.tbody(rows)),
          <.br()
        ).when(state.nonEmpty)
      }.toTagMod
    }

    def renderInfo(name: String, data: Map[ID , SPValue], ids: List[IDAble]): TagMod = {
      val state = data
        .flatMap { case (id, value) => ids.find(_.id == id).map(_ -> value) }
        .toList
        .sortBy { case (idAble, _) => idAble.name }

      val rows = state.map { case (thing, value) =>
        <.tr(
          <.td(thing.name),
          <.td(thing.id.toString),
          <.td(value.toString())
        )
      }.toTagMod

      <.details(
        ^.open := "open",
        Style.collapsible,
        <.summary(name),
        <.table(^.className := "table table-striped Table", <.tbody(rows)),
        <.br()
      ).when(data.nonEmpty)
    }

    def terminateVDs(): Unit = {
      VDCommunication.postRequest(APIVirtualDevice.TerminateAllVDs)
    }

    def terminateAbilities(): Unit = {
      AbilityCommunication.postRequest(APIAbilityHandler.TerminateAllAbilities)
    }

    def terminateDrivers(driverIds : Iterable[DriverId]): Unit = {
      driverIds.foreach { id =>
        DriverCommunication.postRequest(APIDeviceDriver.TerminateDriver(id))
      }
    }

    def terminateRunners(runners: Iterable[RunnerId]): Unit = {
      runners.foreach { runnerId =>
        OperationRunnerCommunication.postRequest(APIOperationRunner.TerminateRunner(runnerId))
      }
    }

    def terminateRunner(id: RunnerId): Unit = {
      OperationRunnerCommunication.postRequest(APIOperationRunner.TerminateRunner(id))
    }
  }

  private val component = ScalaComponent.builder[Props]("VDTracker")
    .renderBackend[Backend]
    .build

  val connectCircuit: ReactConnectProxy[FrontendState] = MainCircuit.connectComponent(identity)

  implicit def toHtml(a: StyleA): TagMod = ^.className := a.htmlClass


  def apply() = SPWidget(_ => connectCircuit { proxy =>  component(Props(proxy)) })
}
