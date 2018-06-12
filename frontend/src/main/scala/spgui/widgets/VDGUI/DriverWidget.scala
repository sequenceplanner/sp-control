package spgui.widgets.VDGUI

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.{VD, APIDeviceDriver, APIVirtualDevice}
import sp.domain._
import spgui.communication._
import spgui.components.SPWidgetElements

object DriverWidget {

  case class Card(driver: VD.Driver, driverState: VD.DriverState, cardId: ID)

  case class State(cards:  List[Card] = List())

  private class Backend($: BackendScope[Unit, State]) {

    val deviceHandler = BackendCommunication.getMessageObserver(onDeviceMessage, APIVirtualDevice.topicResponse)
    val driverHandler = BackendCommunication.getMessageObserver(onDriverMessage, APIDeviceDriver.topicResponse)

    def onDeviceMessage(mess: SPMessage) = {

    }

    def onDriverMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIDeviceDriver.Response].map {
        case APIDeviceDriver.TheDrivers(drivers) => {
          println(drivers.toString)
          $.modState { s =>
            s.copy(cards = drivers.map(d => Card(d._1, d._2, d._1.id)))
          }
        }
        case APIDeviceDriver.TheDriver(d: VD.Driver, driverState: VD.DriverState) => {
          $.modState { s =>
            s.copy(cards = s.cards.filter(c => c.cardId != d.id) :+ Card(d, driverState, d.id))
          }
        }
        case APIDeviceDriver.DriverStateChange(name, id, state, diff) => {
          onDriverStateChange(name, id, state, diff)
        }
        case x => {
          println(x)
          Callback.empty
        }
      }
      callback.foreach(_.runNow())
    }

    def onDriverStateChange(name : String, id : ID , state : VD.DriverState, diff: Boolean) = {
      $.modState(s => s.copy(cards = s.cards.map(c => if(c.driver.id == id) c.copy(driverState = state) else c)))
    }

    def sendToDeviceDriver(mess: APIDeviceDriver.Request) = Callback{
      val h = SPHeader(from = "DriverWidget", to = "DriverService", reply = SPValue("DriverWidget"))
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, APIDeviceDriver.topicRequest)
    }

    def render(s: State) = {
      <.div(
        ^.className := DriverWidgetCSS.rootDiv.htmlClass,
        SPWidgetElements.buttonGroup(Seq(
          SPWidgetElements.button("Get drivers", sendToDeviceDriver(APIDeviceDriver.GetDrivers))
        )),
        SPCardGrid(s.cards.map(c => SPCardGrid.DriverCard(
          cardId = c.cardId,
          name = c.driver.name,
          isOnline = true,
          driverInfo = List(
            "Driver Type: " +c.driver.driverType
          ),
          state = c.driverState.keys.map(k =>
            k.toString + ": " + c.driverState.get(k).getOrElse("Invalid id").toString).toList
        )))
      )
    }

    /*
     send driverID to VDDriverCardsWidget and expand
     When expanded you should be able to:
     1. Force Restart
     2. Force Stop
     3. Force Write
     */
    def renderExpansion(card: Card) = {
      <.div(
        //^.onClick --> onCardClick(card),
        <.div(
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> forceStop(card), "Force Stop"
          ),
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> forceRestart(card), "Force Restart"
          ),
          <.button(
            ^.className := "btn btn-default",
            ^.onClick --> forceWrite(card), "Force Write"
          )
        ),
        <.div(
          "Name:   " + card.driver.name + "\n" +
            "ID:     " + card.driver.id + "\n" +
            //"Online: " + (if (card.driver.driverIsOnline) "Driver Online" else "Driver Offline") + "\n" +
            "Type:   " + card.driver.driverType + "\n" +
            "Setup   " + card.driver.setup + "\n" +
            renderDriverState(card)
        )
      )
    }

    def renderDriverState(card: Card) = {
      // for each element in driverState (Map[String, SPValue])
      // print String, SPValue and a box where we can change SPValue if driver is editable
      // Later: create new driverStates
      card.driverState.toList.map { state: (String, SPValue) =>
        <.div(
          state._1 + "  " + state._2.toString(),
          <.button(
            ^.onClick --> onEditStateClicked(card),
            "Edit SPValue"
          )
        )
      }
    }

    def onEditStateClicked(card: Card) = {
      Callback("DriverWidget: Edit State-Button clicked") // dummy
    }

    /**********ACTIONS**********/

    def onCardClick(card: Card)= {
      <.div(renderExpansion(card))
      // send to widget api that card is clicked
      // handle in BackendComm.MessageObserver that the card should expand/contract
      //Callback("DriverWidget: Card has been clicked") // dummy

      Callback.empty
    }



    /**********CALLBACKS**********/
    /*

    // Todo: Test that the force does what we desire
        should return a message to circuit or backend
     */
    def forceWrite(card: Card) = {
      // callback to backend to write new SPValues to the driver
      sendToDeviceDriver(APIDeviceDriver.DriverCommand(card.driver.id, card.driverState))
      Callback("DriverWidget: Force the driver to write over past state") // dummy
    }
    /*
     force the driver to stop
     */
    def forceStop(card: Card) = {
      // callback to backend to stop the driver
      sendToDeviceDriver(APIDeviceDriver.TerminateDriver(card.driver.id))

      //Callback("DriverWidget: Force the driver to stop") // dummy
    }
    /*
     force the driver to restart
     */
    def forceRestart(card: Card) = {
      // callback to backend to restart the driver
      sendToDeviceDriver(APIDeviceDriver.SetUpDeviceDriver(card.driver))
      Callback("DriverWidget: Force the driver to restart") // dummy
    }

    def onUnmount() = {
      println("DriverWidget Unmouting")
      deviceHandler.kill()
      driverHandler.kill()
      Callback.empty
    }
  }

  private val driverWidgetComponent = ScalaComponent.builder[Unit]("DriverWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => driverWidgetComponent())
}

