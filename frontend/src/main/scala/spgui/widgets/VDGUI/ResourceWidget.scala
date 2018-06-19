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
        case APIVirtualDevice.TheVD(_, _ , resources, _ , _) =>
          $.modState { s => s.copy(resources = resources.map(r => r).toList)}

        // in case of StateEvent from VD,
        // update the resource-state to the
        case APIVirtualDevice.StateEvent(_, id, newState, _) =>
          $.modState { s =>
            s.copy(
              resources = s.resources.map {
                res => if(res.r.id == id) res.copy(state = res.state ++ newState) else res
              }
            )
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
                val relatedDrivers: List[ID] = rws.r.stateMap.map{
                  case mapper:VD.OneToOneMapper => mapper.driverID
                }.distinct
                val selectDrivers: List[(VD.Driver, VD.DriverState, String)] =
                  s.theDrivers.filter(ad => relatedDrivers.contains(ad._1.id))
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

    /**********CALLBACKS**********/
    /*
        force the driver to stop
     */
    def forceStop(card: Card) = {
      // callback to backend to stop the driver
      Callback("ResourceWidget: Force the vd to stop") // dummy
    }

    def onUnmount() = Callback{
      println("ResourceWidget Unmouting")
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
