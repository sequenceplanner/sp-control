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
import spgui.components.{Icon, SPWidgetElements}
import spgui.widgets.itemexplorerincontrol.ModelChoiceDropdown
import spgui.widgets.virtcom.Style
import spgui.components.Button._
import spgui.widgets.VDGUI.cards.CardViewCSS


object VDTrackerWidget {
  import sp.abilityhandler.APIAbilityHandler
  import sp.devicehandler.{APIDeviceDriver, APIVirtualDevice}
  import sp.runners.APIOperationRunner
  import spgui.widgets.VDGUI.{VDTrackerCSS => css}

  case class Props(proxy: ModelProxy[FrontendState]) {
    val activeModel: Option[ModelMock] = proxy.value.models.activeModel
    val activeModelId: Option[ID] = proxy.value.models.activeModelId
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

    def render(props: Props): VdomElement = {
      import sp.models.APIModel

      def onModelClick(modelName: String): Callback = Callback {
        VDTrackerCommunication.postRequest(APIVDTracker.createModel(modelName))
      }

      def onModelChoiceClick(modelId: ID): Callback = {
        props.proxy.dispatchCB(SetActiveModel(modelId)) >>
          Callback { ModelCommunication.postRequest(modelId, APIModel.GetItemList(0, 99999)) }
      }

      def launchAbilities: Callback = Callback {
        props.activeModel.foreach { model =>
          VDTrackerCommunication.postRequest(APIVDTracker.launchVDAbilities(model.items.toList))
        }
      }

      def parseIdAbleData(data: Map[ID, SPValue], ids: SimpleSet[ID, IDAble]): Seq[(IDAble, SPValue)] = {
        data
          .flatMap { case (id, value) => ids.get(id).map(_ -> value) }
          .toList
          .sortBy { case (idAble, _) => idAble.name }
      }

      val models = props.availableVDModels.map { model => SPDropdownItem(model, onModelClick(model)) }
    val idAbles = props.activeModel.map(_.items).getOrElse(SimpleSet[ID, IDAble](_.id))
    val runners = props.runners.toList
    val abilityStates = props.abilities.map(_.state).reduceOption(_ ++ _).getOrElse(Map())
    val abilityTableData = parseIdAbleData(abilityStates, idAbles).map { case (idAble, value) =>
      (idAble.name, shortenID(idAble), splitAbilityValue(value))
    }

    val resourceStates = props.virtualDevices.flatMap(_.resources.toList).map(_.state).reduceOption(_ ++ _).getOrElse(Map())
    val resourceTableData = parseIdAbleData(resourceStates, idAbles).map { case (idAble, value) =>
        (idAble.name, shortenID(idAble), splitAbilityValue(value))
      }


      <.div(
        <.div(
          SPDropdown("Create Model", Callback.empty, Icon.plus, models),
          ModelChoiceDropdown(onModelChoiceClick)
        ),
        <.div(
          <.span(^.marginBottom.:=(2.px),SPButton("Launch VD and Abilities", launchAbilities)),
          <.span(^.marginBottom.:=(2.px),SPButton("Launch operation runner",
            Callback { VDTrackerCommunication.postRequest(APIVDTracker.launchOpRunner(idAbles.toList)) })),
          <.span(^.marginBottom.:=(2.px),SPButton("Terminate Everything", terminateAll(props)))
        ).when(props.activeModelId.isDefined),
        <.div(
          ^.marginTop.:=(2.px),
          props.activeRunnerId.map(activeRunnerId =>
            renderRunners(activeRunnerId, runners, idAbles)).whenDefined,
          renderInfoTable("Ability state", abilityTableData),
          //testStuff(),
          renderInfoTable("Virtual Device state", resourceTableData)
        )
      )
    }

    def shortenID(idAble: IDAble): String = idAble.id.toString.substring(0,6)

    def splitAbilityValue(spVal: SPValue): String = {
      val filter = spVal.toString()
        .filter(_ != '{').filter(_ != '}').filter(_ != ':').filter(_ != ',')
      val split = filter.split('"')
      split.toString
    }

    val tableHeaders = Vector(
      TableRefactor.ColumnData("Name"),
      TableRefactor.ColumnData("Id"),
      TableRefactor.ColumnData("Value")
    )

    def renderInfoTable(summary: TagMod, rows: Seq[Product]): TagMod = {
      <.details(
        css.tableDescriptionText,
        <.summary(Style.collapsible, summary),
        TableRefactor(tableHeaders, rows),
        <.br()
      )
    }

    def renderInfoTable(name: String, rows: Seq[Product]): TagMod = {
      renderInfoTable(<.div(CardViewCSS.cardTitleExpanded, name), rows)
    }

    def terminateAll(props: Props): Callback = Callback {
      terminateAbilities(props)
      terminateDrivers(props.drivers.map(_.id)) // Todo: also remove all drivers from gui disp? */
      terminateVDs(props)
      VDTrackerCommunication.postRequest(APIVDTracker.ResetGUI)

      println("Terminating runners..")
      terminateRunners(props)
    }

    def renderRunners(activeRunnerId: RunnerId, runners: List[Runner], ids: SimpleSet[ID, IDAble]): TagMod = {
      runners.map { runner =>
        val active = runner.id == activeRunnerId
        val state = runner.state

        val rows = state.map { case (id, value) =>
          val name = ids.get(id).map(_.name).getOrElse("")

          <.tr(
            <.td(name),
            <.td(id.toString),
            <.td(value.toString())
          )
        }.toTagMod

        val summaryText = s"Operation runner state (${runner.id})"

        <.details(
          (^.open := "open").when(active),
          ^.className := Style.collapsible.htmlClass,
          <.summary(
            summaryText,
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

    def renderInfo(name: String, data: Map[ID , SPValue], ids: SimpleSet[ID, IDAble]): TagMod = {
      val state = data
        .flatMap { case (id, value) => ids.get(id).map(_ -> value) }
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

    def terminateVDs(props: Props): Unit = {
      VDCommunication.postRequest(APIVirtualDevice.TerminateAllVDs)
      props.proxy.dispatchCB(TerminateAllVirtualDevices).runNow()
    }

    def terminateAbilities(props: Props): Unit = {
      AbilityCommunication.postRequest(APIAbilityHandler.TerminateAllAbilities)
      props.proxy.dispatchCB(TerminateAllAbilities).runNow()
    }

    def terminateDrivers(driverIds : Iterable[DriverId]): Unit = {
      driverIds.foreach { id =>
        DriverCommunication.postRequest(APIDeviceDriver.TerminateDriver(id))
      }
    }

    def terminateRunners(props: Props): Unit = {
      props.proxy.dispatchCB(TerminateAllRunners).runNow()
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
