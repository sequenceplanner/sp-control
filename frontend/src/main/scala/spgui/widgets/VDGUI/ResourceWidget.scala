package spgui.widgets.VDGUI

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.abilityhandler.APIAbilityHandler.Abilities
import sp.devicehandler.{VD, APIVirtualDevice, APIDeviceDriver}
import sp.domain._
import spgui.communication._

object ResourceWidget {

  case class Card(resource: VD.ResourceWithState, cardId: ID)

  case class State(
    resources: List[VD.ResourceWithState] = List(),
    theDrivers: List[(VD.Driver, VD.DriverState, String)] = List()
  )

  private class Backend($: BackendScope[Unit, State]) {

    val deviceHandler = BackendCommunication.getMessageObserver(onDeviceMessage, APIVirtualDevice.topicResponse)
    val driverHandler = BackendCommunication.getMessageObserver(onDriverMessage, APIDeviceDriver.topicResponse)

    def onDriverMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIDeviceDriver.Response].map {
        case APIDeviceDriver.TheDrivers(drivers) => {
          $.modState { s =>
            s.copy(theDrivers = drivers.map(d => (d._1, d._2, d._3)))
          }
        }
        case _ => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    def onDeviceMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIVirtualDevice.Response].map {
        case APIVirtualDevice.TheVD(_, _ , resources, _ , _) => {
          $.modState { s => s.copy(resources = resources.map(r => r).toList)}
        }
        case APIVirtualDevice.StateEvent(resourceName, id, newState, diff) => {
          $.modState { s => s.copy(resources = s.resources.map { res => 
            if(res.r.id == id) res.copy(state = res.state ++ newState)
            else res
          })}
        }
        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    def sendToVirtualDevice(mess: APIVirtualDevice.Request) = Callback{
      val h = SPHeader(from = "ResourceWidget", to = "", reply = SPValue("ResourceWidget"))
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, APIVirtualDevice.topicRequest)
    }

    def render(s: State) = {
      <.div(
        ^.className := DriverWidgetCSS.rootDiv.htmlClass,
        SPCardGrid(
          s.resources.map { rws: VD.ResourceWithState =>
            SPCardGrid.ResourceCard(
              cardId = rws.r.id,
              name = rws.r.name,
              driverStatuses = {
                val relatedDrivers = rws.r.stateMap.map{
                  case mapper:VD.OneToOneMapper => mapper.driverID
                }.distinct
                val selectDrivers = s.theDrivers.filter(ad => relatedDrivers.contains(ad._1.id))
                selectDrivers.map{
                  d => (d._1.name, d._3)
                }.toList
              },
              state = rws.r.stateMap.map {case mapper: VD.OneToOneMapper =>
                (mapper.driverIdentifier.toString, rws.state.get(mapper.thing).get)
              }.toList
            )
          }
        )
      )
    }

    /*
        send driverID to VDDriverCardsWidget and expand
        When expanded you should be able to:
        1. Force Restart
        2. Force Stop
        3. Force Write
     */
    // def renderExpansion(card: Card) = {
    //   <.div(
    //     ^.onClick --> onCardClick(card),
    //     <.div(
    //       <.button(
    //         ^.className := "btn btn-default",
    //         ^.onClick --> forceStop(card), "Force Stop"
    //       )
    //     ),
    //     <.div(
    //       "Name:   " + card.resource.r.name + "\n" +
    //         "ID:     " + card.resource.r.id + "\n" +
    //         "Things:   " + card.resource.r.things + "\n" +
    //         "Setup   " + card.resource.r.setup + "\n" +
    //         renderResourceState(card)
    //     )
    //   )
    // }

    // def renderResourceState(card: Card) = {
    //   // for each element in driverState (Map[String, SPValue])
    //   // print String, SPValue and a box where we can change SPValue if driver is editable
    //   // Later: create new driverStates
    //   <.div("ID" + "      " + "SPValue")
    //   card.resource.state.toList.map { state: (ID, SPValue) =>
    //     <.div(
    //       state._1 + "  " + state._2.toString()
    //     )
    //   }
    // }

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

    def onUnmount() = Callback{
      //println("ResourceWidget Unmouting")
      deviceHandler.kill()
      driverHandler.kill()
    }
  }

  private val resourceWidgetComponent = ScalaComponent.builder[Unit]("ResourceWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.sendToVirtualDevice(APIVirtualDevice.GetVD))
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => resourceWidgetComponent())
}
