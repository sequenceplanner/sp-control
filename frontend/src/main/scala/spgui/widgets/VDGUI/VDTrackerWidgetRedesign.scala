package spgui.widgets.VDGUI

import java.util.{Timer, TimerTask}

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain.{SPValue, _}
import sp.models.APIModel
import sp.vdtesting.APIVDTracker
import spgui.circuits.main.handlers.Aliases._
import spgui.circuits.main.handlers.DriverHandler.DriverId
import spgui.circuits.main.handlers._
import spgui.circuits.main.{FrontendState, MainCircuit}
import spgui.communication._
import spgui.components.SPWidgetElements
import spgui.components.SPWidgetElements.{button, buttonGroup, dropdown}
import spgui.widgets.VDGUI.Table.ColumnData
import spgui.widgets.VDGUI.cards.CardViewCSS
import spgui.widgets.itemexplorerincontrol.ModelChoiceDropdown
import spgui.widgets.virtcom.Style
import spgui.{SPWidget, SimpleSet}

import scala.concurrent.{Future, Promise}
import scala.util.Try

object VDTrackerWidgetRedesign {
  import spgui.widgets.VDGUI.CSSHelpers.toHtml
  import spgui.widgets.VDGUI.{VDTrackerCSS => css}


  import sp.abilityhandler.APIAbilityHandler
  import sp.devicehandler.{APIDeviceDriver, APIVirtualDevice}
  import sp.runners.APIOperationRunner


  // TODO Hack to reduce amount of re-renders. Needs to be changed to something better
  val t = new Timer()

  var reuseProps = true
  var delayRunning = false

  def delay[T](delay: Long)(block: => T): Future[T] = {
    val promise = Promise[T]()
    t.schedule(new TimerTask {
      override def run(): Unit = {
        promise.complete(Try(block))
      }
    }, delay)
    promise.future
  }

  // implicit val reusabilityProxy: Reusability[Props] = Reusability.always
      implicit val reusabilityProxy: Reusability[Props] = Reusability((props, _) => {
        if (!reuseProps) {
          reuseProps = true
          delayRunning = true
          delay(200) {
            //println("Ready")
            reuseProps = false
          }
          false
        } else {
          if (!delayRunning && props.activeModelId.nonEmpty) reuseProps = false
          props.activeModelId.nonEmpty
        }
      })
  // TODO End of hacky re-render optimization

  val tableHeaders = Vector(
    TableRefactor.ColumnData("Name"),
    TableRefactor.ColumnData("Id"),
    TableRefactor.ColumnData("Value")
  )

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

      val idAbles = props.activeModel.map(_.items).getOrElse(SimpleSet[ID, IDAble](_.id))

      val abilityStates = props.abilities.map(_.state).reduceOption(_ ++ _).getOrElse(Map())
      val abilityTableData = parseIdAbleData(abilityStates, idAbles).map { case (idAble, value) =>
        (idAble.name, idAble.id.toString, value.toString())
      }

      val resourceStates = props.virtualDevices.flatMap(_.resources.toList).map(_.state).reduceOption(_ ++ _).getOrElse(Map())
      val resourceTableData = parseIdAbleData(resourceStates, idAbles).map { case (idAble, value) =>
        (idAble.name, idAble.id.toString, value.toString())
      }

      val runnerComponents = props.activeRunnerId.map(id => renderRunners(id, props.runners.toList, idAbles))

      <.div(
        renderMenu(props),
        runnerComponents.whenDefined,
        <.br(),
        renderInfoTable("Ability state", abilityTableData),
        //testStuff(),
        renderInfoTable("Virtual Device state", resourceTableData)
      ).render
    }

    def testStuff(): TagMod = {
      val rows = Seq(
        ("Name 1", "true", "aaaa-bbbb-cccc-aaaa-bbbb-cccc-aaaa-bbbb-cccc-aaaa-bbbb-cccc-"),
        ("Name 2", "{ x: 5 } { x: 5 } { x: 5 } { x: 5 } { x: 5 } { x: 5 }", "aaaa-bbbb-cccc-aaaa-bbbb-cccc-aaaa-bbbb-cccc-aaaa-bbbb-cccc-"),
        ("Name 3", "27", "aaaa-bbbb-cccc-aaaa-bbbb-cccc-aaaa-bbbb-cccc-aaaa-bbbb-cccc-")
      )
      val headers = Vector(
        TableRefactor.ColumnData("Name"),
        TableRefactor.ColumnData("Value"),
        TableRefactor.ColumnData("Id")
      )

      <.details(
        css.tableDescriptionText,
        <.summary(Style.collapsible, "Test"),
        TableRefactor(headers, rows),
        <.br()
      )
    }

    def renderMenu(props: Props): TagMod = {
      val models = props.availableVDModels.map { model => SPWidgetElements.dropdownElement(model, onModelClick(model)) }
      val idAbles = props.activeModel.map(_.items).getOrElse(SimpleSet[ID, IDAble](_.id))

      buttonGroup(Seq(
        dropdown("Create Model", models),
        ModelChoiceDropdown(onModelChoiceClick(props)),
        TagMod(
          button("Launch VD and Abilities", launchAbilities(props)),
          button("Launch operation runner", launchRunner(idAbles)),
          button("Terminate Everything", terminateAll(props))
        ).when(props.activeModelId.isDefined),
        <.br()
      ))
    }

    def launchRunner(idAbles: SimpleSet[ID, IDAble]): Callback = Callback {
      VDTrackerCommunication.postRequest(APIVDTracker.launchOpRunner(idAbles.toList))
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

        val summary = TagMod(
          s"Operation runner state (${runner.id})",
          <.button(
            ^.className := "btn",
            ^.title := "Kill runner",
            ^.onClick --> Callback { terminateRunner(runner.id) },
            <.i(^.className := "fa fa-bolt")
          )
        )
        val rows = parseIdAbleData(state, ids).map { case (idAble, value) =>
          (idAble.name, idAble.id.toString, value.toString())
        }

        <.details(
          (^.open := "open").when(active),
          css.tableDescriptionText,
          <.summary(Style.collapsible, summary),
          TableRefactor(tableHeaders, rows),
          <.br()
        ).when(state.nonEmpty)
      }.toTagMod
    }

    def parseIdAbleData(data: Map[ID, SPValue], ids: SimpleSet[ID, IDAble]): Seq[(IDAble, SPValue)] = {
      data
        .flatMap { case (id, value) => ids.get(id).map(_ -> value) }
        .toList
        .sortBy { case (idAble, _) => idAble.name }
    }

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

    def launchAbilities(props: Props): Callback = Callback {
      props.activeModel.foreach { model =>
        VDTrackerCommunication.postRequest(APIVDTracker.launchVDAbilities(model.items.toList))
      }
    }

    def onModelClick(modelName: String): Callback = Callback {
      VDTrackerCommunication.postRequest(APIVDTracker.createModel(modelName))
    }

    def onModelChoiceClick(props: Props)(modelId: ID): Callback = {
      props.proxy.dispatchCB(SetActiveModel(modelId)) >>
        Callback { ModelCommunication.postRequest(modelId, APIModel.GetItemList(0, 99999)) }
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

  // TODO Issue: previous and current proxy.value is always the same.
  // TODO Perhaps this component should use wrap() instead of connect() for proxy,
  // TODO and then extract parts that need connect() into separate components. Might work.
  private val component = ScalaComponent.builder[Props]("Virtual Device Tracker (Redesign)")
    .renderBackend[Backend]
    .configure(Reusability.shouldComponentUpdate)
    .build

  val connectCircuit: ReactConnectProxy[FrontendState] = MainCircuit.connectComponent(identity)

  def apply() = SPWidget(_ => connectCircuit { proxy =>  component(Props(proxy)) })
}

