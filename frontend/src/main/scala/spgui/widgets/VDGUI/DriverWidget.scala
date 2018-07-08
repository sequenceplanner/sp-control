package spgui.widgets.VDGUI

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.VD
import sp.VDAggregator.APIVDAggregator
import sp.domain._
import spgui.communication._
import spgui.components.SPWidgetElements
import sendMessages._
import sp.vdtesting.APIVDTracker

/** Widget for visualising the drivers status */
object DriverWidget {
  // Information with the driver, the drivers state, status and cardID
  case class Card(driver: VD.Driver, driverState: VD.DriverState, status: String, cardId: ID)
  /** The React-State of the Widget.
    *
    * @param cards List of the cards
    */
  case class State(cards:  List[Card] = List())

  private class Backend($: BackendScope[Unit, State]) {
    val driverHandler =    BackendCommunication.getMessageObserver(onDriverMessage, APIVDAggregator.topicResponse)
    val vdTrackerHandler = BackendCommunication.getMessageObserver(onVDTrackerMessage, APIVDTracker.topicRequest)

    def onVDTrackerMessage(mess: SPMessage) : Unit = {
      mess.getBodyAs[APIVDTracker.Request].map {
        case APIVDTracker.ResetGUI =>
          $.modState ( _.copy(cards = List() ) ).runNow()
        case x => Callback.empty
      }
    }

    /** Handle APIDeviceDriver-messages.
      *
      * If a [[APIVDAggregator.TheDrivers]] response is noticed,
      * add the driver to a card.
      * If something else, Empty Callback.
      * @param mess SPMessage
      */
    def onDriverMessage(mess: SPMessage): Unit = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIVDAggregator.Response].map {
        case APIVDAggregator.TheDrivers(drivers) =>
          $.modState { _.copy(
            cards = drivers.map(d => Card(
              driver = d.driver,
              driverState = d.driverState,
              status = d.status,
              cardId = d.driver.id
            ))
          )}
        case x =>
          Callback.empty
      }
      callback.foreach(_.runNow())
    }

    /** Render-function in Backend.
      *
      * Make a SPCardGrid and for all the cards, map it against a DriverCard.
      *
      * @param state Current State in the Backend-class
      * @return The Widget GUI
      */
    def render(state: State) = {
      <.div(
        SPWidgetElements.button(
          "reload data",
          sendToVDAggregator(APIVDAggregator.GetDrivers)
        ),
        ^.className := DriverWidgetCSS.rootDiv.htmlClass,
        SPCardGrid(state.cards.map(card => SPCardGrid.DriverCard(
          cardId = card.cardId,
          name = card.driver.name,
          status = card.status,
          typ = card.driver.driverType,
          setup = card.driver.setup,
          state = card.driverState
        )))
      )
    }

    /** When the widget is unmounting, kill message-observer
      *
      * @return Callback to kill message-Observers
      */
    def onUnmount: Callback = Callback{
      println("DriverWidget Unmouting")
      driverHandler.kill()
      vdTrackerHandler.kill()
    }

    /** When the widget is mounting, try to get a list of drivers from backend
      *
      * @return Callback
      */
    def onMount: Callback = {
      sendToVDAggregator(APIVDAggregator.GetDrivers)
    }
  }

  private val driverWidgetComponent = ScalaComponent.builder[Unit]("DriverWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount)
    .componentWillUnmount(_.backend.onUnmount)
    .build

  def apply() = spgui.SPWidget(spwb => driverWidgetComponent())
}

