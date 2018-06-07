package spgui.widgets.VDGUI

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.abilityhandler.APIAbilityHandler.Abilities
import sp.devicehandler.{VD, APIVirtualDevice => apiVD}
import sp.domain._
import spgui.communication._

object ResourceWidget {

  case class Card(resource: VD.ResourceWithState)

  case class State(
                    cards:    List[Card]
                  )

  private class Backend($: BackendScope[Unit, State]) {

    val deviceHandler = BackendCommunication.getMessageObserver(onDeviceMessage, apiVD.topicResponse)


    //
    def onDeviceMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[apiVD.Response].map {
        case apiVD.TheVD(_, _ , resources, _ , _) => {
          println("got THE VD res :  "  + resources)
          $.modState { s =>
            s.copy(cards = s.cards ::: resources.map(resourceWithState => Card(resourceWithState)))
          }
        }
        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    def sendToVirtualDeviec(mess: apiVD.Request): Callback = {
      val h = SPHeader(from = "ResourceWidget", to = "", reply = SPValue("ResourceWidget"))
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, apiVD.topicRequest)
      Callback.empty
    }

    def render(s: State) = {
      <.div(
        <.button( ^.className := "btn",
          ^.onClick --> {sendToVirtualDeviec(apiVD.GetVD)}, "Get Virtual Device"
        ),
        <.h1("Resources"),
        s.cards.map { card: Card =>
          <.div(
            ^.onClick --> onCardClick(card),
            "" + card.resource.r.name
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
          )
        ),
        <.div(
          "Name:   " + card.resource.r.name + "\n" +
            "ID:     " + card.resource.r.id + "\n" +
            "Things:   " + card.resource.r.things + "\n" +
            "Setup   " + card.resource.r.setup + "\n" +
            renderResourceState(card)
        )
      )
    }

    def renderResourceState(card: Card) = {
      // for each element in driverState (Map[String, SPValue])
      // print String, SPValue and a box where we can change SPValue if driver is editable
      // Later: create new driverStates
      <.div("ID" + "      " + "SPValue")
      card.resource.state.toList.map { state: (ID, SPValue) =>
        <.div(
          state._1 + "  " + state._2.toString()
        )
      }
    }

    /**********ACTIONS**********/
    def onCardClick(card: Card)= {
      // send to widget api that card is clicked
      // handle in BackendComm.MessageObserver that the card should expand/contract
      Callback("ResourceWidget: Card has been clicked") // dummy
    }

    /**********CALLBACKS**********/
    /*
        force the driver to stop
     */
    def forceStop(card: Card) = {
      // callback to backend to stop the driver
      Callback("ResourceWidget: Force the vd to stop") // dummy
    }

    def onUnmount() = {
      println("ResourceWidget Unmouting")
      deviceHandler.kill()
      Callback.empty
    }
  }

  private val resourceWidgetComponent = ScalaComponent.builder[Unit]("ResourceWidget")
    .initialState(State(List()))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => resourceWidgetComponent())
}



