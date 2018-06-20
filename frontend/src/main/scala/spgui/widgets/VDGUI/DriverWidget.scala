package spgui.widgets.VDGUI

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.{APIDeviceDriver, VD}
import sp.domain._
import spgui.communication._
import spgui.components.SPWidgetElements
import sendMessages._
import sp.vdtesting.APIVDTracker

object DriverWidget {

  // information with the driver, the drivers state, status and cardID
  case class Card(driver: VD.Driver, driverState: VD.DriverState, status: String, cardId: ID)

  case class State(cards:  List[Card] = List())

  private class Backend($: BackendScope[Unit, State]) {
    val driverHandler = BackendCommunication.getMessageObserver(onDriverMessage, APIDeviceDriver.topicResponse)
    val vdTrackingHandler = BackendCommunication.getMessageObserver(onVDTrackerMessage, APIVDTracker.topicRequest)

    def onVDTrackerMessage(mess: SPMessage) : Unit = {
      mess.getBodyAs[APIVDTracker.Request].map {
        case APIVDTracker.ResetGUI =>
          $.modState ( _.copy(cards = List() ) ).runNow()
        case x =>
      }
    }

    def onDriverMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIDeviceDriver.Response].map {
        /**
          * if a [[APIDeviceDriver.TheDrivers]] response is noticed
          * add the driver to a card
          */
        case APIDeviceDriver.TheDrivers(drivers) => {
          $.modState { _.copy(
            cards = drivers.map(d => Card(
              driver = d._1,
              driverState = d._2,
              status = d._3,
              cardId = d._1.id
            ))
          )}
        }

        /**
          * if a [[APIDeviceDriver.DriverStateChange]] response is noticed
          * update the driver in the cards with the help method onDriverStateChange()
          */
        case APIDeviceDriver.DriverStateChange(name, id, state, diff) =>
          onDriverStateChange(name, id, state, diff)

        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    def onDriverStateChange(name : String, id : ID , state : VD.DriverState, diff: Boolean) = {
      $.modState(s => s.copy(cards = s.cards.map(c => if(c.driver.id == id) c.copy(driverState = state) else c)))
    }


    def render(s: State) = {
      <.div(
        ^.className := DriverWidgetCSS.rootDiv.htmlClass,
        SPCardGrid(s.cards.map(c => SPCardGrid.DriverCard(
          cardId = c.cardId,
          name = c.driver.name,
          status = c.status,
          typ = c.driver.driverType,
          setup = c.driver.setup,
          state = c.driverState
        )))
      )
    }

    def onUnmount() = Callback{
      println("DriverWidget Unmouting")
      driverHandler.kill()
    }

    def onMount() = {
      sendToDeviceDriver(APIDeviceDriver.GetDrivers)
    }
  }

  private val driverWidgetComponent = ScalaComponent.builder[Unit]("DriverWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.onMount())
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => driverWidgetComponent())
}

