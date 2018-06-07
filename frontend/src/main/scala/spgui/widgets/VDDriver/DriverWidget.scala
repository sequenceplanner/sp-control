package spgui.widgets.VDDriver

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.VD
import spgui.communication._

import sp.devicehandler.{ APIVirtualDevice => apiVD, APIDeviceDriver => apiDriver }

object DriverWidget {

  case class Card(cardId: String, driver: VD.Driver, isExpanded: Boolean)

  case class State(
                    //driverIdExpanded: ID/driver/driverCard
                    //cardIsExpanded: Boolean
                    drivers:  List[(VD.Driver, VD.DriverState)], // maybe remove and only have a list of cards or vice verse
                    cards:    List[Card]
                  )

  private class Backend($: BackendScope[Unit, State]) {

    val deviceHandler = BackendCommunication.getMessageObserver(onDeviceMessage, apiVD.topicResponse)
    val driverHandler = BackendCommunication.getMessageObserver(onDriverMessage, apiDriver.topicResponse)


    //
    def onDeviceMessage(mess: SPMessage) = {

    }

    def onDriverMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[apiDriver.Response].map{
        case apiDriver.TheDriver(driver, driverState) => {
          $.modState{s =>
            s.copy(drivers = s.drivers :+ (driver, driverState))
          }
        }
        case apiDriver.DriverStateChange(name, id, state, diff) =>{ onDriverStateChange(name, id, state, diff)
        }
        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    def onDriverStateChange(name : String, id : ID , state : VD.DriverState, diff: Boolean) = {
      $.modState(s => s.copy(drivers = s.drivers.map(d => if(d._1.id == id) (d._1, state) else d)))
    }

    
    def sendToDeviceDriver(mess: apiDriver.Request): Callback = {
      val h = SPHeader(from = "DriverWidget", to = "", reply = SPValue("DriverWidget"))
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, apiDriver.topicRequest)
      Callback.empty
    }

    def render(s: State) = {
      <.div(
        <.button( ^.className := "btn",
          ^.onClick --> {sendToDeviceDriver(apiDriver.GetDriver)}, "Get Drivers"
        ),
        <.h1("Driver Names"),
        s.cards.map { card: Card =>
          <.div(
            ^.onClick --> onCardClick(card),
            "" + card.driver.name
          )
        }.toTagMod
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
        ^.onClick --> onCardClick(card),
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
            "Setup   " + card.driver.setup + "\n"
            //+
            //renderDriverState(card.driver.state)
        )
      )
    }

    def renderDriverState(driverState: VD.DriverState) = {
      // for each element in driverState (Map[String, SPValue])
      // print String, SPValue and a box where we can change SPValue if driver is editable
      // Later: create new driverStates
      <.div(

      )
    }

    /**********ACTIONS**********/

    def onCardClick(card: Card)= {
      // send to widget api that card is clicked
      // handle in BackendComm.MessageObserver that the card should expand/contract
      Callback.empty
    }


    /**********CALLBACKS**********/
    /*
        should return a message to circuit or backend
     */
    def forceWrite(card: Card) = {
      // callback to backend to write new SPValues to the driver
      Callback()
    }
    /*
        force the driver to stop
     */
    def forceStop(card: Card) = {
      // callback to backend to stop the driver
      Callback()
    }
    /*
        force the driver to restart
     */
    def forceRestart(card: Card) = {
      // callback to backend to restart the driver
      Callback()
    }

    def terminateDriver(card: Card) = {
      // callback to backend to send APIDeviceDriver.TerminateDriver(id)
      Callback()
    }

    //    def send(mess: apiVD.Request): Callback = {
    //      val h = SPHeader(from = "DriverWidgetService", to = apiVD.service, reply = SPValue("DriverWidgetService"))
    //      val json = SPMessage.make(h, mess) // *(...) is a shorthand for toSpValue(...)
    //      BackendCommunication.publish(json, apiVD.topicRequest)
    //      Callback.empty
    //    }

    def onUnmount() = {
      println("DriverWidget Unmouting")
      Callback.empty
    }
  }

  private val driverWidgetComponent = ScalaComponent.builder[Unit]("DriverWidget")
    .initialState(State(List()))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount)
    .build

  def apply() = spgui.SPWidget(spwb => driverWidgetComponent())
}

