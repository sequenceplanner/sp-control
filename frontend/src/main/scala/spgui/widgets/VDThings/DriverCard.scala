package spgui.widgets.VDThings
import java.util.UUID

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._
import spgui.communication._
import spgui.SPWidget
import sp.domain.logic.StructLogic._
import spgui.components.{Icon, SPWidgetElements}

/*
The driverCard shows the driver name, id and if its online.
If clicked, send ID to VDDriverCardsWidget and expand.

 */
object DriverCard {
  case class Props(
                    driverName:   String,
                    driverID:     UUID,
                    isOnline:     Boolean
                  )
  case class State(
                    expanded:        Boolean
                  )

  private class Backend($: BackendScope[Props, State]) {


    def render(p: Props, s: State) = {
      <.div(
        if(s.expanded) renderExpansion()
        else renderCard(p)
      )
    }

    /*
        render the card with its name etc
     */
    def renderCard(p: Props) = {
      <.div(
        <.div(
          <.h2(
            p.driverName
          )
        )
      )
    }

    /*
        send driverID to VDDriverCardsWidget and expand
        When expanded you should be able to:
        1. Force Restart
        2. Force Stop
        3. Force Write
     */
    def renderExpansion() = {
      <.div(
        <.div(
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> forceStop(), "Force Stop"
          ),
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> forceRestart(), "Force Restart"
          ),
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> forceWrite(), "Force Write"
          )
        )
      )
    }

    /*
        should return a message to circuit or backend
     */
    def forceWrite() = ???
    /*
        force the driver to stop
     */
    def forceStop() = ???
    /*
        force the driver to restart
     */
    def forceRestart() = ???


    def onUnmount(): Callback =  {
      Callback.empty
    }
  }

  val driverCardComponent = ScalaComponent.builder[Props]("DriverCard")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply() = ???
}

