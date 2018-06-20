package spgui.widgets.VDGUI

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.{VD, APIDeviceDriver}
import sp.domain._
import spgui.communication._
import spgui.components.SPWidgetElements
import sendMessages._


object DriverWidget {

  // information with the driver, the drivers state, status and cardID
  case class Card(driver: VD.Driver, driverState: VD.DriverState, status: String, cardId: ID)

  case class State(cards:  List[Card] = List())

  private class Backend($: BackendScope[Unit, State]) {
    val driverHandler = BackendCommunication.getMessageObserver(onDriverMessage, APIDeviceDriver.topicResponse)

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
        case APIDeviceDriver.DriverStateChange(name, id, state, diff) => {
          onDriverStateChange(name, id, state, diff)
        }

        case APIDeviceDriver.DriverCommandDone(id, result) => {
          Callback.empty
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
        SPWidgetElements.button(
          "reload data",
          sendToDeviceDriver(APIDeviceDriver.GetDrivers)
        ),
        ^.className := DriverWidgetCSS.rootDiv.htmlClass,
        SPCardGrid(s.cards.map(c => SPCardGrid.DriverCard(
          cardId = c.cardId,
          name = c.driver.name,
          status = c.status,
          typ = c.driver.driverType,
          setup = c.driver.setup,
          state = c.driverState//c.driverState.keys.map(k =>(k.toString, c.driverState.get(k).get)).toList
        )))
      )
    }

    /**********ACTIONS**********/
    /* "DriverWidget: Edit State-Button clicked") // dummy
       TODO: Should edit one state of the driver */
    def onEditStateClicked(card: Card) = ???


    def forceWrite(card: Card) = ???
    /*{
      // callback to backend to write new SPValues to the driver
      sendToDeviceDriver(APIDeviceDriver.DriverCommand(card.driver.id, card.driverState))
      Callback("DriverWidget: Force the driver to write over past state") // dummy
    }*/

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

