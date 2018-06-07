package spgui.widgets.VDGUI

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.abilityhandler.{ APIAbilityHandler => apiAH }
import sp.domain._
import spgui.communication._

object AbilityWidget {

  case class Card(ability: apiAH.Ability)

  case class State(
                    cards:    List[Card]
                  )

  private class Backend($: BackendScope[Unit, State]) {

    val abilityHandler = BackendCommunication.getMessageObserver(onAbilityMessage, apiAH.topicResponse)
    //
    def onAbilityMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[apiAH.Response].map {
        case apiAH.Abilities(abilities) => {
          $.modState { s =>
            s.copy(cards = s.cards ::: abilities.map(ability => Card(ability)))
          }
        }
        case x => Callback.empty
      }
    }

    def sendToAbilityHandler(mess: apiAH.Request): Callback = {
      val h = SPHeader(from = "AbilityWidget", to = "Derp", reply = SPValue("AbilityWidget"))
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, apiAH.topicRequest)
      Callback.empty
    }

    def render(s: State) = {
      <.div(
        <.button( ^.className := "btn",
          ^.onClick --> {sendToAbilityHandler(apiAH.GetAbilities)}, "Get Abilities"
        ),
        <.h1("Abilities"),
        s.cards.map { card: Card =>
          <.div(
            ^.onClick --> onCardClick(card),
            "" + card.ability.name
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
            "Name:   " + card.ability.name + "\n" +
            "ID:     " + card.ability.id + "\n"
        )
      )
    }

    /**********ACTIONS**********/
    def onCardClick(card: Card)= {
      // send to widget api that card is clicked
      // handle in BackendComm.MessageObserver that the card should expand/contract
      Callback("AbilityWidget: Card has been clicked") // dummy
    }

    /**********CALLBACKS**********/
    /*
        force the driver to stop
     */
    def forceStop(card: Card) = {
      // callback to backend to stop the driver
      Callback("AbilityWidget: Force the vd to stop") // dummy
    }

    def onUnmount() = {
      println("AbilityWidget Unmouting")
      abilityHandler.kill()
      Callback.empty
    }
  }

  private val driverWidgetComponent = ScalaComponent.builder[Unit]("AbilityWidget")
    .initialState(State(List()))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => driverWidgetComponent())
}





