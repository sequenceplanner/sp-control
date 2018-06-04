package spgui.widgets.VDThings
import java.util.UUID

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._
import spgui.communication._
import spgui.SPWidget
import diode.react.ModelProxy
import sp.devicehandler.APIDeviceDriver


object VDDriverCardsWidget {
  case class Props(
                  //drivers: ModelProxy[SOMETHING?]
                  )
  case class State(
                    idExpanded:   UUID,   // the id of the driver expanded
                    isExpanded:   Boolean // boolean if a driver is expanded

                  )

  private class Backend($: BackendScope[Props, State]) {
    val abilityHandler =
      BackendCommunication.getMessageObserver(onDriverMessage, APIDeviceDriver.topicResponse)

    /*
        onDriverMessage should the drivers from props update and re-render
        Call renderCards if changed in circuit/backend
     */
    def onDriverMessage(mess: SPMessage) = {

    }

    /*
        when mounted, search after the drivers in circuit/backend from model
        update drivers in Props
     */
    def onMount() = {

    }
    def onUnmount() = {
      Callback.empty
    }

    /*
          render all drivers from props
          call renderCards
      */
    def render(p: Props, s: State) = {

    }

    // use DriverCard to visualize and create the different DriverCard:s from this widget
    def renderCards(
                    //List or Map of DriverCards
                   ) = {

    }
  }

  val driverCardsComponent = ScalaComponent.builder[Props]("VDDriverCardsWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount())
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = SPWidget(swpb => driverCardsComponent())

}

