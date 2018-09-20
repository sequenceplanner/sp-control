package spgui.widgets.labkit

import scala.util.{Success, Failure}
import scala.concurrent.Future

import java.util.UUID
import japgolly.scalajs.react._

import japgolly.scalajs.react.vdom.html_<^._

import sp.domain._
import sp.domain.Logic._

import monocle.macros._
import monocle.Lens

import diode.react.ModelProxy
import spgui.circuits.availablemodelscircuit.{ AvailableModelsCircuit, AvailableModels }
import spgui.components.{ SPWidgetElements }
import spgui.communication.APIComm
import spgui.communication.APIComm._

object LabkitExperimentWidget {
  import sp.abilityhandler.{APIAbilityHandler => apiab}
  import sp.operationmatcher.{API => apiom}
  import sp.models.{APIModel => apimodel}
  import sp.patrikmodel.{API => apipm}

  case class Props(mp: ModelProxy[AvailableModels])
  case class State(s: List[String]=List(),
    abs: List[apiab.Ability]=List(),
    manualModels: List[String] = List(),
    selectedModel: Option[ID] = None
  )
  private class Backend($: BackendScope[Props, State]) {
    import scala.concurrent.ExecutionContext.Implicits.global
    val lp = Operation("lf1LoadPart", attributes = SPAttributes("pairs" -> Map("group"->"lf1", "type"->"addProduct", "trigger"->"x")))
    val cc = Operation("lf1CloseClamps", attributes = SPAttributes("pairs" -> Map("group"->"lf1", "type"->"clamping")))
    val oc = Operation("lf1OpenClamps", attributes = SPAttributes("pairs" -> Map("group"->"lf1", "type"->"clamping")))

    val lp2 = Operation("lf2LoadPart", attributes = SPAttributes("pairs" -> Map("group"->"lf2", "type"->"addProduct", "trigger"->"x")))
    val cc2 = Operation("lf2CloseClamps", attributes = SPAttributes("pairs" -> Map("group"->"lf2", "type"->"clamping")))
    val oc2 = Operation("lf2OpenClamps", attributes = SPAttributes("pairs" -> Map("name"->"lf2_openClamps")))

    val ops = List(lp,cc,oc, lp2, cc2, oc2)

    val widgetName = "labkitExperimentWidget"

    // setup communication with backend APIs
    val pmcomm = new APIComm[apipm.Request, apipm.Response](apipm.topicRequest,
      apipm.topicResponse, widgetName, apipm.service, Some(() => pmOnChannelUp()), None)

    val modelcomm = new APIComm[apimodel.Request, apimodel.Response](apimodel.topicRequest,
      apimodel.topicResponse, widgetName, apimodel.service, None, None)

    val omcomm = new APIComm[apiom.Request, apiom.Response](apiab.topicRequest,
      apiab.topicResponse, widgetName, apiab.service, None, None)

    def pmOnChannelUp(): Unit = {
      pmcomm.request(apipm.GetAvailableModels).takeFirstResponse.foreach {
        case (_,apipm.AvailableModels(models)) =>
          $.modState(s => s.copy(manualModels = models)).runNow()
        case x =>
      }
    }

    def doOMTest() = {
      ops.foreach { op =>
        val pairs = op.attributes.getAs[Map[String, SPValue]]("pairs").getOrElse(Map())
        omcomm.request(apiom.Find(pairs)).takeFirstResponse.onComplete {
          case Success((header,apiom.Matches(matches, neighbors))) =>
            $.modState{s =>
              val abnames = matches.map(_.name)
              val neighbornames = neighbors.map(_.name)
              s.copy(s = (header.reqID.toString + ": " + abnames.mkString(",") + " ~ " + neighbornames.mkString(",")) :: s.s)
            }.runNow()
          case Failure(err) =>
            $.modState(s => s.copy(s = err.toString :: s.s)).runNow()
          case x =>
        }
      }
      Callback.empty
    }

    def createPatrikModel(m: String) = {
      pmcomm.request(apipm.CreateManualModel(m)).takeFirstResponse.foreach {
        case (_,apipm.ManualModel(ids)) =>
          val stateChange = $.modState{ s =>
            s.copy(s = "got idables: " + ids.toString :: s.s)
          }
          val saveToModel = $.state >>= { s =>
            s.selectedModel.foreach { m =>
              modelcomm.request(SPHeader(from = widgetName, to = m.toString), apimodel.PutItems(ids)).doit.foreach(x=>())
            }
            Callback.empty
          }
          (stateChange >> saveToModel).runNow()
        case x =>
      }
    }

    def render(p: Props, s: State) = {
      <.div(
        <.button(
          ^.className := "btn btn-default", <.i(^.className := "fa fa-bolt"),
          ^.onClick --> doOMTest(),
          " Test matching"
        ),
        SPWidgetElements.dropdown(
          s.selectedModel.map(_.toString).getOrElse("Select a model"),
          p.mp().models.toSeq.map{ case(id,name) => <.div(s"${name} (${id.toString})", ^.onClick --> $.modState(s => s.copy(selectedModel = Some(id))))}),
        <.table(
          ^.className := "table table-striped",
          <.tbody(s.manualModels.map(m=>
            <.tr(
              <.td(m),
              <.td(<.button(
                ^.className := "btn btn-default",
                ^.onClick --> Callback(createPatrikModel(m)),
                <.i(^.className := "fa fa-magic")
              )))).toTagMod)),
        <.table(
          ^.className := "table table-striped",
          <.tbody(s.s.map(m=> <.tr(<.td(m))).toTagMod))
      )
    }

    def onUnmount() = {
      modelcomm.kill
      omcomm.kill
      pmcomm.kill
      Callback.empty
    }

    def onMount() = {
      Callback.empty
    }

  }

  val avmcConnection = AvailableModelsCircuit.connect(x => x)

  private val component = ScalaComponent.builder[Props]("LabkitExperiment")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount())
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => avmcConnection(proxy => component(Props(proxy))))
}
