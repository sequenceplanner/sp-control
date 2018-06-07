package spgui.widgets.VDDriver

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.{VD, APIDeviceDriver => apiDriver, APIVirtualDevice => apiVD}
import sp.domain._
import spgui.communication._

object DriverWidget {

  case class Card(cardId: String, driver: VD.Driver, driverState: VD.DriverState, isExpanded: Boolean)

  case class State(
                    //driverIdExpanded: ID/driver/driverCard
                    //cardIsExpanded: Boolean
                    expandedCard: Option[ID] = None,
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


    val idA = ID.newID
    val idB = ID.newID
    val idC = ID.newID

    def render(s: State) = {
      <.div(
        ^.className := DriverWidgetCSS.rootDiv.htmlClass,
        <.button( ^.className := "btn",
          ^.onClick --> {sendToDeviceDriver(apiDriver.GetDriver)}, "Get Drivers"
        ),
        // <.h1("Driver Names"),
        // s.cards.map { card: Card =>
        //   <.div(
        //     ^.onClick --> onCardClick(card),
        //     "" + card.driver.name
        //   )
        // }.toTagMod,
        cardGroup(
          List(
            renderCard(s.expandedCard,idA, "Nomen", true, false, "Lorem ipsum dolor sit amet",
              List("a: yes", "b: maybe", "c: no")),
            renderCard(s.expandedCard,idB, "Nomen", true, false, "Lorem ipsum dolor sit amet",
              List("a: yes", "b: maybe", "c: no")),
            renderCard(s.expandedCard,idC, "Nomen", true, false, "Lorem ipsum dolor sit amet",
              List("a: yes", "b: maybe", "c: no")),

          )
        )
      )
    }

    /*
     Inner Components
     •	Driver Name
     •	Color
     •	Green (if online)
     •	Red (if offline)
     •	(Driver info)
     •	(Current State)
     •	(Force restart/stop)
     •	(Force Write)
     */

    def cardGroup(cards: List[TagMod]): TagMod = <.div(
      ^.className := DriverWidgetCSS.cardGroup.htmlClass,
      ^.className := "input-group",
      cards.toTagMod
    )

    def renderCard(
      expandedId: Option[ID],
      cardId: ID,
      name: String,
      isOnline: Boolean,
      isExpanded: Boolean,
      driverInfo: String,
      state: List[String]
    ): TagMod = <.span(
      ^.className := DriverWidgetCSS.cardOuter.htmlClass,
      <.div(
        ^.className := DriverWidgetCSS.cardTitle.htmlClass,
        name
      ),
      ^.onClick --> $.modState(s =>
        if(s.expandedCard == Some(cardId)) s.copy(expandedCard = None)
        else s.copy(expandedCard = Some(cardId))
      ),
      {
        if(!expandedId.isEmpty){
          val exist = cardId == expandedId.get
          exist match {
            case true => ^.className := DriverWidgetCSS.cardExpanded.htmlClass
            case false => ^.className := DriverWidgetCSS.cardCollapsed.htmlClass
          }
        }
        else EmptyVdom
      }
    )


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
      // send to widget api that card is clicked
      // handle in BackendComm.MessageObserver that the card should expand/contract
      Callback("DriverWidget: Card has been clicked") // dummy
    }


    /**********CALLBACKS**********/
    /*
        should return a message to circuit or backend
     */
    def forceWrite(card: Card) = {
      // callback to backend to write new SPValues to the driver
      Callback("DriverWidget: Force the driver to write over past state") // dummy
    }
    /*
        force the driver to stop
     */
    def forceStop(card: Card) = {
      // callback to backend to stop the driver
      Callback("DriverWidget: Force the driver to stop") // dummy
    }
    /*
        force the driver to restart
     */
    def forceRestart(card: Card) = {
      // callback to backend to restart the driver
      Callback("DriverWidget: Force the driver to restart") // dummy
    }

    def terminateDriver(card: Card) = {
      // callback to backend to send APIDeviceDriver.TerminateDriver(id)
      Callback("DriverWidget: Force the driver to terminate") // dummy
    }

    //    def send(mess: apiVD.Request): Callback = {
    //      val h = SPHeader(from = "DriverWidgetService", to = apiVD.service, reply = SPValue("DriverWidgetService"))
    //      val json = SPMessage.make(h, mess) // *(...) is a shorthand for toSpValue(...)
    //      BackendCommunication.publish(json, apiVD.topicRequest)
    //      Callback.empty
    //    }

    def onUnmount() = {
      println("DriverWidget Unmouting")
      deviceHandler.kill()
      driverHandler.kill()
      Callback.empty
    }
  }

  private val driverWidgetComponent = ScalaComponent.builder[Unit]("DriverWidget")
    .initialState(State(None, List(),List()))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => driverWidgetComponent())
}

