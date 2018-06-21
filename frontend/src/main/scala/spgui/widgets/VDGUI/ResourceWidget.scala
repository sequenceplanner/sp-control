package spgui.widgets.VDGUI

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.abilityhandler.APIAbilityHandler.Abilities
import sp.devicehandler.{VD, APIVirtualDevice, APIDeviceDriver}
import sp.domain._
import spgui.communication._

/** Widget to visualize the Resources and it's status*/
object ResourceWidget {
  case class State(
                    resources: List[VD.ResourceWithState] = List(),
                    theDrivers: List[(VD.Driver, VD.DriverState, String)] = List()
                  )

  private class Backend($: BackendScope[Unit, State]) {

    val deviceHandler = BackendCommunication.getMessageObserver(onDeviceMessage, APIVirtualDevice.topicResponse)
    val driverHandler = BackendCommunication.getMessageObserver(onDriverMessage, APIDeviceDriver.topicResponse)

    /** Handle APIDeviceDriver-messages.
      *
      * If a [[APIDeviceDriver.TheDrivers]] response is noticed,
      * update the local list of driver.
      *
      * If something else, Empty Callback.
      *
      * @param mess SPMessage from APIDeviceDriver
      */
    def onDriverMessage(mess: SPMessage): Unit = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIDeviceDriver.Response].map {
        case APIDeviceDriver.TheDrivers(drivers) => {
          $.modState { s =>
            s.copy(theDrivers = drivers.map(d => (d._1, d._2, d._3)))
          }
        }
        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    /** Handle APIVirtualDevice-messages.
      *
      * If a [[APIVirtualDevice.TheVD]] response is noticed,
      * update the local list of resources.
      *
      * If a [[APIVirtualDevice.StateEvent]] response is noticed,
      * update the local state of resource with same ID.
      *
      * If something else, Empty Callback.
      *
      * @param mess SPMessage from APIDeviceDriver
      */
    def onDeviceMessage(mess: SPMessage): Unit = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIVirtualDevice.Response].map {
        case APIVirtualDevice.TheVD(_, _, newResources, _ , _) =>
          $.modState { s: State => s.copy(resources = s.resources ++ newResources)}

        // in case of StateEvent from VD,
        // update the resource-state to the new value
        case APIVirtualDevice.StateEvent(_, id, newState, _) =>
          $.modState { s: State =>
            s.copy(resources = s.resources.map { res =>
                if(res.r.id == id) res.copy(state = res.state ++ newState) else res})
          }

        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    /** Set SPHeader and send SPMessage to APIVirtualDevice
      *
      * @param mess SPMessage of APIVirtualDevice
      */
    def sendToVirtualDevice(mess: APIVirtualDevice.Request): Unit = {
      val header = SPHeader(from = "ResourceWidget", to = "", reply = SPValue("ResourceWidget"))
      BackendCommunication.publish(SPMessage.make(header, mess), APIVirtualDevice.topicRequest)
    }

    /** Render-function in Backend.
      *
      * Make a SPCardGrid and for all the resources in state, map it against a ResourceCard.
      *
      * @param state Current state in Backend-class
      * @return The Widget GUI
      */
    def render(state: State) = {
      <.div(
        ^.className := DriverWidgetCSS.rootDiv.htmlClass,
        SPCardGrid(
          state.resources.map { rws: VD.ResourceWithState =>
            SPCardGrid.ResourceCard(
              cardId = rws.r.id,
              name = rws.r.name,
              driverStatuses = {
                val relatedDrivers: List[ID] = rws.r.stateMap.map{
                  case mapper:VD.OneToOneMapper => mapper.driverID
                }.distinct
                val selectDrivers: List[(VD.Driver, VD.DriverState, String)] =
                  state.theDrivers.filter{
                    driver: (VD.Driver, VD.DriverState, String) => relatedDrivers.contains(driver._1.id)
                  }
                selectDrivers.map{
                  driver => (driver._1.name, driver._3)
                }
              },
              state = rws.r.stateMap.map {case mapper: VD.OneToOneMapper =>
                val a: SPValue = rws.state.get(mapper.thing).get
                val b: SPValue = rws.state(mapper.thing)
                println(s".get(id).get = $a and .(key) = $b")
                  (mapper.driverIdentifier.toString, rws.state.get(mapper.thing).get)
              }
            )
          }
        )
      )
    }

    /**********CALLBACKS**********/
    /** When the widget is unmounting, kill message-observer.
      *
      * @return Callback
      */
    def onUnmount: Callback = Callback{
      println("ResourceWidget Unmouting")
      deviceHandler.kill()
      driverHandler.kill()
    }

    /** When the widget did mount, try to get all VD that is active.
      *
      * @return Callback
      */
    def didMount: Callback = Callback{
      println(s"Resource Widget didMount and is requesting GetVD")
      sendToVirtualDevice(APIVirtualDevice.GetVD)
    }
  }

  private val resourceWidgetComponent = ScalaComponent.builder[Unit]("ResourceWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.didMount)
    .componentWillUnmount(_.backend.onUnmount)
    .build

  def apply() = spgui.SPWidget(spwb => resourceWidgetComponent())
}
