package spgui.widgets.robotservices

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import spgui.communication._

// Import this to make SPAttributes work including json handling
import sp.domain._
import Logic._


import sp.robotservices.{APIRobotServices => api}



  object RobotLogServiceWidget {

    case class State(pathModules: String, pathLogs: String, pathWorkCells: String, running:Boolean)

    private class Backend($: BackendScope[Unit, State]) {

      val messObs = BackendCommunication.getMessageObserver(
        mess => {
          val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[api.Response].map {
            case api.Finished =>
                $.modState(s => s.copy(running = false))

            case api.Started =>
              $.modState (s =>s.copy(running = true))
          }
          callback.foreach(_.runNow())

        },
        api.topicResponse
      )

      def pathLogChange(s: State)(e: ReactEventFromInput) = {
        $.modState(s=>s.copy(pathLogs = e.target.value)).runNow()
        Callback.empty
      }
      def pathModChange(s: State)(e: ReactEventFromInput) = {
        $.modState(s=>s.copy(pathModules = e.target.value)).runNow()
        Callback.empty
      }
      def pathWorkChange(s: State)(e: ReactEventFromInput) = {
        $.modState(s=>s.copy(pathWorkCells = e.target.value)).runNow()
        Callback.empty
      }
      def render(s: State) = {
        <.div(
          <.h2("Running? " + (if(s.running) "yes" else "no")),
          <.br(),
          <.input(
            ^.value     := s.pathLogs,
            ^.onChange ==> pathLogChange(s)
          ),
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> send(api.LoadLog(s.pathLogs)), "Load robot Logs"
          ),
          <.br(),
          <.input(
            ^.value     := s.pathModules,
            ^.onChange ==> pathModChange(s)
          ),
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> send(api.LoadRobotModules(s.pathModules)), "Load robot modules"
          ),
          <.br(),
          <.input(
            ^.value     := s.pathWorkCells,
            ^.onChange ==> pathWorkChange(s)
          ),
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> send(api.LoadWorkCells(s.pathWorkCells)), "Load workcells"
          ),
          <.br(),
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> send(api.PlayLogs), "Play log"
          ),
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> send(api.StopPlayingLogs), "Stop log"
          ),
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> send(api.Connect), "Connect"
          )
        )
      }

      def onUnmount() = {
        println("Unmounting")
        messObs.kill()
        Callback.empty
      }


      def send(mess: api.Request): Callback = {
        val h = SPHeader(from = "robotLogServiceWidget", to = api.logPlayer, reply = SPValue("robotLogServiceWidget"))
        val json = SPMessage.make(h, mess) // *(...) is a shorthand for toSpValue(...)
          BackendCommunication.publish(json, api.topicRequest)
        Callback.empty
      }


    }


    private val component = ScalaComponent.builder[Unit]("RobotLogServiceWidget")
      .initialState(State( pathLogs= "/home/ashfaqf/Projects/Lisa files/from_volvo/logs/20-10-2017/logs/log-1754060_10_18_15_55", pathModules = "/home/ashfaqf/Projects/Lisa files/from_volvo/logs/20-10-2017/RobotPrograms/1754000", pathWorkCells ="/home/ashfaqf/Projects/Lisa files/from_volvo/logs/20-10-2017/workCells", running = false))
      .renderBackend[Backend]
      .componentWillUnmount(_.backend.onUnmount())
      .build

    def apply() = spgui.SPWidget(spwb => component())
  }
