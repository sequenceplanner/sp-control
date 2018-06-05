package spgui.widgets.VDDriver

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.APIDeviceDriver
import sp.devicehandler.VD.DriverState
import sp.domain._

/*
The driverCard shows the driver name, id and if its online.
If clicked, send ID to VDDriverCardsWidget and expand.

 */
object DriverCard {
  case class State(
                    driverName:       String,
                    driverID:         ID,
                    driverType:       String,
                    driverSetup:      SPAttributes,
                    driverState:      DriverState,
                    driverIsOnline:   Boolean,
                    cardIsExpanded:   Boolean,
                    driverIsEditalbe: Boolean
                  )

  private class Backend($: BackendScope[Unit, State]) {

    /**********RENDERING**********/
    def render(s: State) = {
      <.div(
        if(s.cardIsExpanded) renderExpansion(s)
        else renderCard(s)
      )
    }

    /*
        render the card with its name etc
     */
    def renderCard(s: State) = {
      <.div(
        ^.onClick --> cardClick(s),
        <.div(
          <.h2(
            s.driverName
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
    def renderExpansion(s: State) = {
      <.div(
        ^.onClick --> cardClick(s),
        <.div(
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> forceStop(s), "Force Stop"
          ),
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> forceRestart(s), "Force Restart"
          ),
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> forceWrite(s), "Force Write"
          )
        ),
        <.div(
          "Name:   " + s.driverName + "\n" +
          "ID:     " + s.driverID + "\n" +
          "Online: " + (if (s.driverIsOnline) "Driver Online" else "Driver Offline") + "\n" +
          "Type:   " + s.driverType + "\n" +
          "Setup   " + s.driverSetup + "\n" +
          renderDriverState(s.driverState)
        )
      )
    }

    def renderDriverState(driverState: DriverState) = {
      // for each element in driverState (Map[String, SPValue])
      // print String, SPValue and a box where we can change SPValue if driver is editable
      // Later: create new driverStates
      <.div(

      )
    }

    /**********ACTIONS**********/
    def cardClick(s: State) = {
      // Callback to DriversLogic or DriverWidget that card is expanded?
      invertExpanded(s)
    }

    def invertExpanded(s: State) = {
      $.modState(s => s.copy(cardIsExpanded = !s.cardIsExpanded))
    }

    def invertEditable(s: State) = {
      $.modState(s => s.copy(driverIsEditalbe = !s.driverIsEditalbe))
    }

    /**********CALLBACKS**********/
    /*
        should return a message to circuit or backend
     */
    def forceWrite(s: State) = {
      // callback to DriversLogic to write new SPValues to the driver with s.driverID
      Callback()
    }
    /*
        force the driver to stop
     */
    def forceStop(s: State) = {
      // callback to DriversLogic to stop the driver with s.driverID
      Callback()
    }
    /*
        force the driver to restart
     */
    def forceRestart(s: State) = {
      // callback to DriversLogic to restart the driver with s.driverID
      Callback()
    }

    def terminateDriver(s: State) = {
      // callback to DriversLogic to send APIDeviceDriver.TerminateDriver(id)
      Callback()
    }

    def onUnmount(): Callback =  {
      Callback.empty
    }
  }

  val driverCardComponent = ScalaComponent.builder[Unit]("DriverCard")
    .initialState(State())
    .renderBackend[Backend]
    .build

  def apply() = driverCardComponent()
}

