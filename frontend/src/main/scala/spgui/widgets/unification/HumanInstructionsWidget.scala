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
  case class State(name: String = "Kristofer",
                   cmd: Option[Map[String, SPValue]] = None,
                   ack: Boolean = false,
                   completed: Boolean = false,
                   header: Option[SPHeader] = None
                  )



  private class Backend($: BackendScope[Unit, State]) {

    val driverHandler =
      BackendCommunication.getMessageObserver(onDriverMessage, APIHumanDriver.topicToHuman)

    def onDriverMessage(mess: SPMessage): Unit = {
      for {
        h <- mess.getHeaderAs[SPHeader]
        b <- mess.getBodyAs[APIHumanDriver.ToHuman]
      } yield {
        b match {
          case x: APIHumanDriver.StateChangeRequest =>
            $.modState{s =>
              val updS = s.copy(
                cmd = Some(x.state),
                ack = false,
                completed = false,
                header = Some(h)
              )

              sendEvent(updS, false, false)

              updS
            }.runNow()

        }
      }
      for {
        h <- mess.getHeaderAs[SPHeader]
        b <- mess.getBodyAs[APIHumanDriver.FromHuman]
      } yield {
        b match {
          case y: APIHumanDriver.HumanEvent =>
            $.modState{s =>
              val updS = s.copy(
                cmd = Some(y.state),
                ack = s.ack,
                completed = s.completed,
                header = Some(h)
              )

              sendEvent(updS, updS.ack, updS.completed)

              updS
            }.runNow()


        }
      }

    }

    def render(p:Unit, s:State) = {
      <.div(
        <.h1(s"The human ${s.name}" ),
        s.cmd.map { cmd =>
          <.div(
            <.div(cmd.toString),
            <.br(),
            <.div(if (s.ack) "ack" else ""),
            <.br(),
            <.div(if (s.completed) "completed" else "")
          )
        }.getOrElse(<.div("")),
        <.br(),

        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> sendEvent(s, true, s.completed), "Ack"
        ),

        <.button(
          ^.className := "btn btn-default",
          ^.onClick --> sendEvent(s, s.ack, true), "done"
        )
      )
    }


    def onUnmount(): Callback =  {
      driverHandler.kill
      Callback.empty
    }

    def sendEvent(s: State, ack: Boolean = false, done: Boolean): Callback = {
      val humanS = s.cmd.getOrElse(Map()) ++ Map[String, SPValue](
        "ack" -> ack, "completed" ->  done
      )

      val header = s.header.map(_.swapToAndFrom).getOrElse(SPHeader(from = "THE HUMAN WIDGET"))

      val json = SPMessage.make(header, APIHumanDriver.HumanEvent(s.name, humanS)) // *(...) is a shorthand for toSpValue(...)
      BackendCommunication.publish(json, APIHumanDriver.topicFromHuman)

      val updCmd = if (done) None else s.cmd

      $.modState(x => x.copy(cmd = updCmd, ack =ack, completed = done, header = None))
    }
  }


  private val component = ScalaComponent.builder[Unit]("HumanInstruction")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = SPWidget(swpb => component())
}

