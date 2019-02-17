package spgui.widgets.MiniModelHelper

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalacss.internal.StyleA
import sp.domain.{SPValue, _}
import sp.domain.APISP._
import spgui.{SPWidget, SimpleSet}
import spgui.circuits.main.handlers.Aliases._
import spgui.circuits.main.{FrontendState, MainCircuit}
import spgui.circuits.main.handlers._
import spgui.communication._
import spgui.components.SPWidgetElements
import spgui.widgets.itemexplorerincontrol.ModelChoiceDropdown
import spgui.widgets.virtcom.Style
import sp.modelSupport.APIMiniModelService
import sp.runners.APIRunnerManager

object MiniModelHelperWidget {

  import sp.modelSupport.{APIMiniModelService => api}
  import spgui.communication.APIComm._
  import scala.concurrent.ExecutionContext.Implicits.global

  val comm = new APIComm[api.Request, api.Response](
    requestTopic = api.topicRequest,
    responseTopic = api.topicResponse,
    from = "MiniModelHelperService",
    to = api.service,
    onChannelUp = None,
    onMessage = None
  )

  case class Props(proxy: ModelProxy[FrontendState]) {
    val activeModel: Option[ModelMock] = proxy.value.models.activeModel
    val activeModelId: Option[ID] = proxy.value.models.activeModelId
    val activeRunnerId: Option[RunnerId] = proxy.value.runners.latestActiveRunnerId
    val runnerStates: Map[ID, Map[ID, SPValue]] = proxy.value.runners.runnerStates

    def models: SimpleSet[ID, ModelMock] = proxy.value.models.models
    def availableMiniModels: List[String] = proxy.value.runners.availableMiniModels
  }

  case class State(ltl: String = "", ltlresult: String = "")

  private class Backend($: BackendScope[Props, State]) {
    def render(props: Props, s: State): VdomElement = {
      import SPWidgetElements.{button, buttonGroup, dropdown}
      import sp.models.APIModel

      def onModelClick(modelName: String): Callback = Callback {
        MiniModelHelperCommunication.postRequest(APIMiniModelService.createModel(modelName))
      }

      def onModelChoiceClick(modelId: ID): Callback = {
        props.proxy.dispatchCB(SetActiveModel(modelId)) >>
          Callback { ModelCommunication.postRequest(modelId, APIModel.GetItemList(0, 99999)) }
      }

      def onLTLChange(e: ReactEventFromInput) = {
        val newValue = e.target.value
          $.modState(_.copy(ltl = newValue))
      }

      def setplan = {
        val plan = s.ltlresult.split("\n").toList.filter(_.contains("_start = TRUE")).map(_.trim)
        val ops = props.activeModel.map(_.items.toList).getOrElse(List()).collect { case op: Operation => op }
        val opNames = ops.map(o => "o_"+o.name.replaceAll("\\.", "_") -> o.name).toMap
        val p = plan.flatMap( str => opNames.get(str.stripSuffix("_start = TRUE")))

        props.activeRunnerId.foreach { runnerID => send(APIRunnerManager.SetPlan(runnerID, p)) }

        $.modState(_.copy(ltlresult = p.mkString("\n")))
      }

      def check = {
        for {
          idables <- props.activeModel.map(_.items.toList)
          runnerID <- props.activeRunnerId
          runnerState <- props.runnerStates.get(runnerID)
        } yield {
          // this is a terrible hack. shame!
          $.modState(s => s.copy(ltlresult="computing...")).runNow()
          val x= comm.request(api.bmc(idables, runnerState, s.ltl, 50)).map {
            case (header, Left(SPError(err, attr))) =>
              println("got error!: " + err)
              $.modState(s => s.copy(ltlresult="error...")).runNow()
              None
            case (header,Right(api.bmcOutput(stdout))) =>
              println("GOT REPLY!: " + stdout)
              Some(stdout)
            case x =>
              println("GOT OTHER REPLY!: " + x)
              None
          }
          x.runLog.unsafeRunAsync { case Right(y) =>
            val x = y.flatMap(x=>x).fold(""){ case (a,b) => a++b }
            $.modState(s => s.copy(ltlresult=x.replaceAll("(?m)^\\*\\*\\*.*?\n", ""))).runNow()
          }
        }
      }

      val models = props.availableMiniModels.map { model => SPWidgetElements.dropdownElement(model, onModelClick(model)) }
      val idAbles = props.activeModel.map(_.items).getOrElse(SimpleSet[ID, IDAble](_.id))

      <.div(
        buttonGroup(Seq(
          dropdown("Create Model", models),
          ModelChoiceDropdown(onModelChoiceClick),
          TagMod(
            button("Terminate Everything", terminateAll(props))
          ).when(props.activeModelId.isDefined)
        )),
        <.br(),
        <.input(
          ^.width := "80%",
          ^.value := s.ltl,
          ^.onChange ==> onLTLChange
        ),
        <.button(
          ^.className := "btn",
          ^.title := "Check",
          ^.onClick --> Callback(check),
          <.i(^.className := "fa fa-circle")
        ),
        <.button(
          ^.className := "btn",
          ^.title := "Set plan",
          ^.onClick --> setplan,
          <.i(^.className := "fa fa-bars")
        ),
        <.br(),
        <.pre(s.ltlresult)
      ).render
    }

    def terminateAll(props: Props): Callback = Callback {
      //      terminateAbilities(props)
      //      terminateDrivers(props.drivers.map(_.id)) // Todo: also remove all drivers from gui disp? */
      //      terminateVDs(props)

      println("Terminating runners..")
      //      terminateRunners(props)
    }

    // def renderRunners(activeRunnerId: RunnerId, runners: List[Runner], ids: SimpleSet[ID, IDAble]): TagMod = {
    //   runners.map { runner =>
    //     val active = runner.id == activeRunnerId
    //     val state = runner.state

    //     val rows = state.map { case (id, value) =>
    //         val name = ids.get(id).map(_.name).getOrElse("")

    //         <.tr(
    //           <.td(name),
    //           <.td(id.toString),
    //           <.td(value.toString())
    //         )
    //     }.toTagMod

    //     rows
    //   }.toTagMod
    // }

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
      RunnerManagerCommunication.postRequest(APIRunnerManager.TerminateAllRunnerInstances)
    }
  }

  def send(mess: APIRunnerManager.Request): Callback = {
    RunnerManagerCommunication.postRequest(mess)
    Callback.empty
  }

  private val component = ScalaComponent.builder[Props]("VDTracker")
    .initialState(State(""))
    .renderBackend[Backend]
    .build

  val connectCircuit: ReactConnectProxy[FrontendState] = MainCircuit.connectComponent(identity)

  implicit def toHtml(a: StyleA): TagMod = ^.className := a.htmlClass


  def apply() = SPWidget(_ => connectCircuit { proxy =>  component(Props(proxy)) })
}
