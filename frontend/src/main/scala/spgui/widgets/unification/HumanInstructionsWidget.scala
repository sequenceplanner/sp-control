package spgui.widgets.unification

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain.Logic._
import sp.domain.{SPMessage, SPValue, _}
import spgui.SPWidget
import spgui.communication._
import spgui.components.{SPWidgetElements => Comp}

object HumanInstructionsWidget {
  //import sp.devicehandler._
  import sp.driver._

  // TODO: Handle different persons...
  case class State(humans: Map[ID, APIHumanDriver.HumanStateMessage])

  private class Backend($: BackendScope[Unit, State]) {

    val driverHandler =
      BackendCommunication.getMessageObserver(onDriverMessage, APIHumanDriver.topicToHuman)


    def onDriverMessage(mess: SPMessage): Unit = {
      for {
        h <- mess.getHeaderAs[SPHeader]
        b <- mess.getBodyAs[APIHumanDriver.ToHuman]
      } yield {
        b match {
          case x: APIHumanDriver.HumanStateMessage =>
            $.modState{s =>
              val updS = State(s.humans + (x.driverID -> x))
              updS
            }.runNow()
        }
      }
    }

    def render(p:Unit, s:State): VdomElement = {
      <.div(
        s.humans.toList.map{case (id, h) => TagMod(// Should only be one human in unification demo
           if (h.alert.nonEmpty) TagMod(
             <.h1(h.alert),
             <.button(
               ^.className := "btn btn-warning",
               ^.onClick --> sendEvent(h, h.ack, h.done, ""), "Completed"
             ))
           else EmptyVdom,
           if (!h.loggedIn) {
              <.div(
                <.h1(s"Need to log in" ),
                <.div(h.toString)
              )
            } else {
              <.div(
                <.h1(s"${h.humanName} is logged in"),
                <.div(h.toString),
                <.br(),
                if (h.cmd.nonEmpty){
                  TagMod(
                    <.div(s"Operation: ${h.cmd}"),
                    <.div(s"${h.instructions.getOrElse(h.cmd, "No description for this OP")}")
                  )
                } else {
                  <.div("Have a break, no instructions now")
                },
                <.button(
                  {if (h.ack) ^.className := "btn btn-success"
                  else ^.className := "btn btn-default"},
                  ^.onClick --> sendEvent(h, true, h.done, h.alert), "Ack"
                ),

                <.button(
                  {if (h.done) ^.className := "btn btn-success"
                  else ^.className := "btn btn-default"},
                  ^.onClick --> sendEvent(h, h.ack, true, h.alert), "done"
                )
              )
            })
        }:_*
      )
    }


    def onUnmount(): Callback =  {
      driverHandler.kill
      println("SIMPLE TEST")
      Callback.empty
    }

    def sendEvent(h: APIHumanDriver.HumanStateMessage, ack: Boolean, done: Boolean, alert: String): Callback = {
      val header = SPHeader(from = "THE HUMAN WIDGET")
      val mess = APIHumanDriver.HumanEvent(h.driverID, ack && h.cmd.nonEmpty, done && h.cmd.nonEmpty, alert)

      val json = SPMessage.make(header, mess)
      BackendCommunication.publish(json, APIHumanDriver.topicFromHuman)
      Callback.empty
    }
  }


  private val component = ScalaComponent.builder[Unit]("HumanInstruction")
    .initialState(State(Map()))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = SPWidget(swpb => component())
}

