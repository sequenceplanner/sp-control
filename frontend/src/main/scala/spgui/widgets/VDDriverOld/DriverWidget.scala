package spgui.widgets.VDDriverOld

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.domain._




object DriverWidget {
  case class State(
                    driverCards:  List[DriverCardTrait],
                    idExpanded:   ID,   // the id of the driver expanded. Useful?
                    isExpanded:   Boolean //   boolean if a driver is expanded. Useful?
                  )

  class Backend($: BackendScope[Unit, State]) {

    /*
          render all drivers from props
          call renderCards
      */
    def render(s: State) = {
      <.div(
        if (s.isExpanded) renderExpandedCard(s.idExpanded)
        else renderCards(s.driverCards)
      )
    }

    // use DriverCard to visualize and for all the different DriverCard:s in the list => print
    def renderCards(
                    cards: List[DriverCardTrait]
                   ) = {
      <.div(

      )
    }

    // ToDo: Communicate with the specific DriverCard with the same driverID?
    def renderExpandedCard(driverID: ID) = ???

    /*
        ToDo: In the future we want the to create new drivers from this widget
     */
    def createDriver(): Unit = ???

    def onUnmount() = {
      Callback.empty
    }
  }


  val driverCardsComponent = ScalaComponent.builder[Unit]("VDDriverCardsWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = SPWidget(swpb => driverCardsComponent())

}

