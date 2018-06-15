package spgui.widgets.VDGUI

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.{VD, APIDeviceDriver}
import sp.domain._
import spgui.communication._
import spgui.components.SPWidgetElements
import sendMessages._


object DriverWidget {

  case class Card(driver: VD.Driver, driverState: VD.DriverState, status: String, cardId: ID)

  case class State(cards:  List[Card] = List())

  private class Backend($: BackendScope[Unit, State]) {
    val driverHandler = BackendCommunication.getMessageObserver(onDriverMessage, APIDeviceDriver.topicResponse)

    def onDriverMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIDeviceDriver.Response].map {
        /**
          * if a [[APIDeviceDriver.TheDrivers]] response is noticed
          * add the drivers to a card
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
        case APIDeviceDriver.DriverStateChange(name, id, state, diff) => {
          onDriverStateChange(name, id, state, diff)
        }
        case x => {
          Callback.empty
        }
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
          state = c.driverState.keys.map(k =>(k.toString, c.driverState.get(k).get)).toList
        )))
      )
    }

    /**********ACTIONS**********/
    /* "DriverWidget: Edit State-Button clicked") // dummy
       TODO: Should edit one state of the driver */
    def onEditStateClicked(card: Card) = ???

    /**********BACKEND CALLS**********/
    /*

        Todo: Test that the force does what we desire
        should return a message to circuit or backend
     */
    def forceWrite(card: Card) = ???
    /*{
      // callback to backend to write new SPValues to the driver
      sendToDeviceDriver(APIDeviceDriver.DriverCommand(card.driver.id, card.driverState))
      Callback("DriverWidget: Force the driver to write over past state") // dummy
    }*/
    /*
      TODO: force the driver to go offline/terminate
     */
    def forceStop(card: Card) = ???
    /*{
      // callback to backend to stop the driver
      sendToDeviceDriver(APIDeviceDriver.TerminateDriver(card.driver.id))
    }*/
    /*
       TODO: force the driver to restart
     */
    def forceRestart(card: Card) = ???
    /*{
      // callback to backend to restart the driver
      sendToDeviceDriver(APIDeviceDriver.SetUpDeviceDriver(card.driver))
      Callback("DriverWidget: Force the driver to restart") // dummy
    }*/

    def onUnmount() = {
      println("DriverWidget Unmouting")
      driverHandler.kill()
      Callback.empty
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

