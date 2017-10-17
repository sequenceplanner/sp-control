package spgui.widgets.examples

import java.util.UUID
import japgolly.scalajs.react._

import japgolly.scalajs.react.vdom.all.{ a, h1, h2, href, div, className, onClick, br, key }
import japgolly.scalajs.react.vdom.html_<^._

import sp.domain._
import sp.domain.Logic._

import monocle.macros._
import monocle.Lens

object LabkitExperimentWidget {
  import sp.abilityhandler.{APIAbilityHandler => apiab}
  import sp.operationmatcher.{API => apiom}
  import spgui.communication.{BackendCommunication => bc }


  case class State(s: List[String]=List(), abs: List[apiab.Ability]=List())
  private class Backend($: BackendScope[Unit, State]) {
    val lp = Operation("lf1LoadPart", attributes = SPAttributes("pairs" -> Map("group"->"lf1", "type"->"addProduct", "trigger"->"x")))
    val cc = Operation("lf1CloseClamps", attributes = SPAttributes("pairs" -> Map("group"->"lf1", "type"->"clamping")))
    val oc = Operation("lf1OpenClamps", attributes = SPAttributes("pairs" -> Map("group"->"lf1", "type"->"clamping")))

    val lp2 = Operation("lf2LoadPart", attributes = SPAttributes("pairs" -> Map("group"->"lf2", "type"->"addProduct", "trigger"->"x")))
    val cc2 = Operation("lf2CloseClamps", attributes = SPAttributes("pairs" -> Map("group"->"lf2", "type"->"clamping")))
    val oc2 = Operation("lf2OpenClamps", attributes = SPAttributes("pairs" -> Map("name"->"lf2_openClamps")))

    val ops = List(lp,cc,oc, lp2, cc2, oc2)

    val abHandler = bc.getMessageObserver(abHandlerCB, apiab.topicResponse)
    def abHandlerCB(mess: SPMessage): Unit = {
      val header = mess.header.to[SPHeader].getOrElse(SPHeader())

      mess.body.to[apiab.Response].map{
        case apiab.Abilities(abs) =>
          $.modState(s => s.copy(abs = abs)).runNow()
        case x =>
      }

      mess.body.to[apiom.Response].map{
        case apiom.Matches(matches, neighbors) =>
          $.modState{s =>
            val abnames = matches.map(_.name)
            val neighbornames = neighbors.map(_.name)
            s.copy(s = (header.reqID.toString + ": " + abnames.mkString(",") + " ~ " + neighbornames.mkString(",")) :: s.s)
          }.runNow()
        case x =>
      }
    }

    def sendToAH(body: apiab.Request) = {
      val msg = SPMessage.make[SPHeader, apiab.Request](SPHeader(to = apiab.service, from = "hej"), body)
      bc.publish(msg, apiab.topicRequest)
    }

    def sendToOM(body: apiom.Request) = {
      val msg = SPMessage.make[SPHeader, apiom.Request](SPHeader(to = apiab.service, from = "hej"), body)
      bc.publish(msg, apiab.topicRequest)
    }

    def doTest() = {
      ops.foreach { op =>
        val pairs = op.attributes.getAs[Map[String, SPValue]]("pairs").getOrElse(Map())
        sendToOM(apiom.Find(pairs))
      }
      Callback.empty
    }


    def render(s: State) = {
      <.div(
        <.button(
          ^.className := "btn btn-default", <.i(^.className := "fa fa-bolt"),
          ^.onClick --> doTest(),
          " Create test operations"
        ),
        <.table(
          ^.className := "table table-striped",
          <.tbody(s.s.map{m=>
            <.tr(<.td(m))}.toTagMod)
        )
      )
    }

    def onUnmount() = {
      abHandler.kill()
      Callback.empty
    }

    def onMount() = {
      Callback.empty
    }

  }

  private val component = ScalaComponent.builder[Unit]("LabkitExperiment")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount())
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => component())
}
