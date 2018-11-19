package spgui.widgets.examples

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import spgui.SPWidgetBase
import spgui.components.SPWidgetElements
import spgui.communication._
import spgui.communication.APIComm._
import sp.domain._
import Logic._
import spgui.circuits.main.{FrontendState, MainCircuit}
import spgui.circuits.main.handlers._
import scala.util.Try
import scalajs.js
import js.JSON
import play.api.libs.json._


object URDemoWidget {
  import spgui.widgets.examples.{RunnerStateCSS => css}
  import sp.drivers.ros2.{APIRosFrontendHelper => api }

  case class State(hasBlue: Boolean = false, hasRed: Boolean = false, hasYellow: Boolean = false, hasGreen: Boolean = false, urSpeed: Double = 0.1)

  class Backend($: BackendScope[Unit, State]) {
    import scala.concurrent.ExecutionContext.Implicits.global
    val ros2comm = new APIComm[api.Request, api.Response](api.topicRequest, api.topicResponse, "URDemoWidget", api.service, None, None)

    def publish(s: State) = {
      val str = "hasBlue="+s.hasBlue.toString+":hasRed="+s.hasRed.toString+":hasYellow="+s.hasYellow.toString+":hasGreen="+s.hasGreen.toString
      val msg = SPAttributes("data" -> str)
      ros2comm.request(api.Publish("std_msgs/String", "/unification_roscontrol/demo_human", msg)).doit.foreach {
        case x => println(x)
      }
    }

    def sendReset() = {
      val msg = SPAttributes("ref_pos" -> "reset")
      ros2comm.request(api.Publish("unification_ros2_messages/MoveItSPToUni", "/unification_roscontrol/ur_moveit_sp_to_unidriver", msg)).doit.foreach {
        case x => println(x)
      }
      Callback.empty
    }

    def setProps(speedScaling: Double) = {
      import spgui.communication.{BackendCommunication => bc }
      val state = Map("speed_scaling" -> SPValue(speedScaling),
        "acc_scaling" -> SPValue(speedScaling))
      val msg = SPMessage.make(SPHeader(), state)
      bc.publish(msg, "urProps")
    }

    def toggleBlue = {
      $.modState{s =>
        val newState = s.copy(hasBlue = !s.hasBlue)
        publish(newState)
        newState
      }
    }

    def toggleRed = {
      $.modState{s =>
        val newState = s.copy(hasRed = !s.hasRed)
        publish(newState)
        newState
      }
    }

    def toggleYellow = {
      $.modState{s =>
        val newState = s.copy(hasYellow = !s.hasYellow)
        publish(newState)
        newState
      }
    }

    def toggleGreen = {
      $.modState{s =>
        val newState = s.copy(hasGreen = !s.hasGreen)
        publish(newState)
        newState
      }
    }

    def onSpeedChange(e: ReactEventFromInput) = {
      val newValue = e.target.value.toDouble / 100.0
      setProps(newValue)
      $.modState(_.copy(urSpeed = newValue))
    }

    def render(s: State) = {
      val hasBlueCSS =
        if (s.hasBlue) css.activeModelButton.htmlClass
        else css.inactiveModelButton.htmlClass

      val hasRedCSS =
        if (s.hasRed) css.activeModelButton.htmlClass
        else css.inactiveModelButton.htmlClass

      val hasYellowCSS =
        if (s.hasYellow) css.activeModelButton.htmlClass
        else css.inactiveModelButton.htmlClass

      val hasGreenCSS =
        if (s.hasGreen) css.activeModelButton.htmlClass
        else css.inactiveModelButton.htmlClass


      <.div(
        <.label("Human has blue"),
        <.button(
          ^.className := s"btn btn-sm ${hasBlueCSS}",
          ^.title := "Human has blue brick",
          ^.onClick --> toggleBlue,
          <.i(^.className := (if (s.hasBlue) "fa fa-circle" else "fa fa-circle-thin"))),

        <.label("Human has red"),
        <.button(
          ^.className := s"btn btn-sm ${hasRedCSS}",
          ^.title := "Human has red brick",
          ^.onClick --> toggleRed,
          <.i(^.className := (if (s.hasRed) "fa fa-circle" else "fa fa-circle-thin"))),

        <.label("Human has yellow"),
        <.button(
          ^.className := s"btn btn-sm ${hasYellowCSS}",
          ^.title := "Human has yellow brick",
          ^.onClick --> toggleYellow,
          <.i(^.className := (if (s.hasYellow) "fa fa-circle" else "fa fa-circle-thin"))),

        <.label("Human has green"),
        <.button(
          ^.className := s"btn btn-sm ${hasGreenCSS}",
          ^.title := "Human has green brick",
          ^.onClick --> toggleGreen,
          <.i(^.className := (if (s.hasGreen) "fa fa-circle" else "fa fa-circle-thin"))),

        <.br(),
        <.div(
          <.label("UR Speed"),
          <.input(
            ^.width := "150px",
            ^.`type` := "range",
            ^.min := 0,
            ^.max := 50,
            ^.step := 1,
            ^.value := s.urSpeed * 100,
            ^.onChange ==> onSpeedChange
          )
        ),
        <.br(),
        <.button(
          ^.className := "btn btn-small",
          ^.onClick --> sendReset, "Send reset to UR"
        )
      )
    }
  }

  private val component = ScalaComponent.builder[Unit]("Ros2")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply() = spgui.SPWidget(_ => component())
}
