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
            case _ => Callback.empty
          }
         callback.runNow()
        }
      },
        api.topicResponse
      )

      def renderState(p: Props, s: State) = {
        <.table(
          ^.width:="900px",
          <.caption("Runner state"),
          <.thead(
            <.tr(
              <.th(^.width:="400px","Name"),
              <.th(^.width:="200px","Value"),
            )
          ),
          <.tbody(
            p.activeModel.map { m =>
              s.state.map { case (id, v) =>
                m.items.get(id).map(_.name).getOrElse(id.toString) -> v
              }.toList.sortBy(_._1).map { case (name, v) =>
                  val internalValue = 0
                  <.tr(
                    <.td(name),
                    <.td(v.toString),
                    <.td(
                      <.input(
                        ^.width := "80px",
                        ^.value     := internalValue,
                        // ^.onChange ==> updateInternalValue(s, n.name)
                      ),
                      <.button(
                        ^.width := "70px",
                        ^.className := "btn btn-small",
                        ^.onClick --> Callback.empty, "write"
                      )
                    ))
                }
            }.getOrElse(List()).toTagMod
          )
        )
      }

      def render(p: Props, s: State) = {
        <.div(
          <.button(
            ^.className := "btn btn-small",
            ^.onClick --> send(api.StartAuto), "start auto"
          ),
          <.button(
            ^.className := "btn btn-small",
            ^.onClick --> send(api.StopAuto), "stop auto"
          ),
          renderState(p, s)
        )
      }

      def onUnmount() = {
        println("Unmounting")
        //messObs.kill()
        Callback.empty
      }
    }

    def send(mess: api.Request): Callback = {
      VDCommunication.postRequest(mess)
      Callback.empty
    }

    val connectCircuit: ReactConnectProxy[FrontendState] = MainCircuit.connectComponent(identity)

    private val component = ScalaComponent.builder[Props]("RunnerStateWidgetState")
      .initialState(State(Map()))
      .renderBackend[Backend]
      .componentWillUnmount(_.backend.onUnmount())
      .build

    def apply() = spgui.SPWidget(_ => connectCircuit { proxy =>  component(Props(proxy)) })

  }
