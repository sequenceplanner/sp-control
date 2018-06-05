package spgui.widgets.VDDriver

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.APIDeviceDriver
import sp.domain._



object DriverWidget {
  case class Props(
                  //drivers: ModelProxy[SOMETHING?]
                  )
  case class State(
                    idExpanded:   ID,   // the id of the driver expanded
                    isExpanded:   Boolean // boolean if a driver is expanded
                  )

  private class Backend($: BackendScope[Props, State]) {

    val driverListHandler =
      BackendCommunication.getMessageObserver(onDriverListMessage, APIDeviceDriver.topicResponse)

    /*
        onDriverMessage see if drivers is added or removed from circuit/Backend
     */
    def onDriverListMessage(mess: SPMessage) = ???

    /*
        when mounted, search after the drivers in circuit/backend from model
        update drivers in Props
     */
    def onMount() = {

    }
    def onUnmount() = {
      driverListHandler.kill()
      Callback.empty
    }

    /*
          render all drivers from props
          call renderCards
      */
    def render(p: Props, s: State) = {
      <.div(
        if (s.isExpanded) renderExpandedCard(s.idExpanded)
        else renderCards()
      )
    }

    // use DriverCard to visualize and create the different DriverCard:s from this widget
    def renderCards(
                    //List or Map of DriverCards
                   ) = {
      <.div(

      )
    }

    // ToDo: Communicate with the specific DriverCard with the same driverID
    def renderExpandedCard(driverID: ID) = ???

    /*
        ToDo: In the future we want the to create new drivers from this widget
     */
    def createDriver(): Unit = ???
  }


  val driverCardsComponent = ScalaComponent.builder[Props]("VDDriverCardsWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount())
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = SPWidget(swpb => driverCardsComponent())

}

