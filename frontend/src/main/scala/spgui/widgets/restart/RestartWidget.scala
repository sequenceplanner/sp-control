package spgui.widgets.restart

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import spgui.SPWidgetBase
import spgui.components.SPWidgetElements
import spgui.communication._
import sp.domain._
import Logic._
import spgui.circuits.main.{FrontendState, MainCircuit}
import sp.runners.{APIRunnerManager => api}
import spgui.circuits.main.handlers._

case class Props(proxy: ModelProxy[FrontendState]) {
  val activeModel: Option[ModelMock] = proxy.value.models.activeModel
  val activeRunner: Option[ID] = proxy.value.runners.latestActiveRunnerId
  val runnerStates: Map[ID, Map[ID, SPValue]] = proxy.value.runners.runnerStates
}

object RestartWidget {
  import spgui.widgets.examples.{RunnerStateCSS => css}

  val typeFilters = List("type", "operation", "ability", "internal", "input", "output")

  case class State(isAuto: Boolean = false, isStepping: Boolean = false, forceGoal: String = "", forceGoalActive: Boolean = false)

  private class Backend($: BackendScope[Props, State]) {

    def toggleAuto(runnerID: ID) = {
      $.modState{s =>
        val newState = s.copy(isAuto = !s.isAuto)
        if(newState.isAuto)
          send(api.StartAuto(runnerID))
        else send(api.StopAuto(runnerID))
        newState
      }
    }

    def toggleStepping(runnerID: ID) = {
      $.modState{s =>
        val newState = s.copy(isStepping = !s.isStepping)
        send(api.SetStepping(newState.isStepping, runnerID))
        newState
      }
    }

    def toggleForceGoal(runnerID: ID) = {
      $.modState{s =>
        val newState = s.copy(forceGoalActive = !s.forceGoalActive)
        val toSend = if(newState.forceGoalActive) Some(s.forceGoal) else None
        send(api.SetForceGoal(runnerID, toSend))
        newState
      }
    }

    def toggleButton(title: String, c: CallbackTo[Unit], isToggled: Boolean) = {
      val buttonCss =
        if (isToggled) css.activeModelButton.htmlClass
        else css.inactiveModelButton.htmlClass
      val imageCss =
        if (isToggled) "fa fa-4x fa-circle"
        else "fa fa-2x fa-circle-thin"

      <.button(
        ^.className := s"btn btn-sm ${buttonCss}",
        ^.title := title,
        ^.onClick --> c,
        <.i(^.className := imageCss))
    }

    def onForceGoalChange(e: ReactEventFromInput) = {
      val newValue = e.target.value
      $.modState(_.copy(forceGoal = newValue, forceGoalActive = false))
    }

    def renderQueue(rid: ID, p: Props, s: State) = {
      val rows = for {
        state <- p.runnerStates.get(rid)
      } yield {
        state.values
      }
      val tags = rows.toList.map { r =>
        val q = r.flatMap(v => v.getAs[List[String]]("q")).headOption.getOrElse(List("no active queue"))
        val g = r.flatMap(v => v.getAs[String]("goal")).headOption.getOrElse(List("no active goal"))
        List(<.div(^.fontSize := "18px")("Current goal: " + g),
          <.div(^.fontSize := "12px")("Current queue: " + q.map(_.stripSuffix("_pre")).mkString(",")))
        }
        tags.flatten.toTagMod
      }

      def render(p: Props, s: State) = {
        // TODO: show abilities so we can force run them...
        val abilities = p.activeModel.flatMap(m => m.items.find(_.name == "abilities").
          flatMap(_.attributes.getAs[List[Operation]]("modelAbilities"))).getOrElse(List())

        <.div(
          p.activeRunner.map { runnerID =>
            <.div(^.fontSize := "18px")(
              "Active runner: " + runnerID.toString,

              <.br(),
              toggleButton("Control in auto", toggleAuto(runnerID), s.isAuto), <.b("Control in auto: "),

              <.br(),
              toggleButton("Step abilities", toggleStepping(runnerID), s.isStepping), <.b("Step abilities: "),
              <.button(
                ^.className := "btn btn-small",
                ^.onClick --> Callback(send(api.TakeStep(runnerID))), <.i(^.className := "fa fa-4x fa-play")
              ).when(s.isStepping),

              <.br(),
              toggleButton("Force goal", toggleForceGoal(runnerID), s.forceGoalActive),
              <.label("Force goal:"),
            <.input(
              ^.width := "500px",
              ^.value := s.forceGoal,
              ^.onChange ==> onForceGoalChange
            ),
            renderQueue(runnerID, p, s),
          )
        }.toList.toTagMod,
      )
    }

    def onUnmount() = {
      println("Unmounting")
      //messObs.kill()
      Callback.empty
    }
  }

  def send(mess: api.Request) = {
    RunnerManagerCommunication.postRequest(mess)
  }

  val connectCircuit: ReactConnectProxy[FrontendState] = MainCircuit.connectComponent(identity)

  private val component = ScalaComponent.builder[Props]("RestartWidgetState")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(_ => connectCircuit { proxy =>  component(Props(proxy)) })

}
