package spgui.widgets.examples

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import spgui.SPWidgetBase
import spgui.communication._
import sp.domain._
import Logic._
import spgui.circuits.main.{FrontendState, MainCircuit}
import sp.devicehandler.{APIVirtualDevice => api}
import spgui.circuits.main.handlers._

  case class Props(proxy: ModelProxy[FrontendState]) {
    val activeModel: Option[ModelMock] = proxy.value.models.activeModel
  }

  object RunnerStateWidgetState {
    case class State(state: Map[ID, SPValue])

    private class Backend($: BackendScope[Props, State]) {

      val messObs = BackendCommunication.getMessageObserver( mess => {

        for {
          b <- mess.getBodyAs[api.Response]
        } yield {
          val callback = b match {
            case api.StateEvent(_, _, state, _) =>
              $.modState{s =>
                State(s.state ++ state)
              }
          }
         callback.runNow()
        }
      },
        api.topicResponse
      )

      def render(p: Props, s: State) = {
        <.div(
          <.h1(s"Runner state:"),
          s.state.map { case (id, v) =>
            val name = p.activeModel.flatMap(m => m.items.get(id).map(_.name)).getOrElse(id.toString)
            <.div(name + "--" + v.toString)
          }.toTagMod,
        )
      }

      def onUnmount() = {
        println("Unmounting")
        //messObs.kill()
        Callback.empty
      }
    }

    val connectCircuit: ReactConnectProxy[FrontendState] = MainCircuit.connectComponent(identity)

    private val component = ScalaComponent.builder[Props]("RunnerStateWidgetState")
      .initialState(State(Map()))
      .renderBackend[Backend]
      .componentWillUnmount(_.backend.onUnmount())
      .build

    def apply() = spgui.SPWidget(_ => connectCircuit { proxy =>  component(Props(proxy)) })

  }
