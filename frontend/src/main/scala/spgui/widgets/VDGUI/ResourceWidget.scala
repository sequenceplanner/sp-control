package spgui.widgets.VDGUI

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.abilityhandler.APIAbilityHandler.Abilities
import sp.devicehandler.VD.{OneToOneMapper, Resource, ResourceWithState}
import sp.devicehandler.{APIDeviceDriver, APIVirtualDevice, VD}
import sp.domain._
import spgui.communication._
import spgui.widgets.VDGUI.SPCardGrid.RenderCard

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
                if(res.resource.id == id) res.copy(state = res.state ++ newState) else res})
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
      val cards = state.resources.map { resourceData =>
        renderResourceCard(state, resourceData.resource, resourceData.state)
      }

      <.div(
        ^.className := DriverWidgetCSS.rootDiv.htmlClass,
        SPCardGrid(cards)
      )
    }

    def renderResourceCard(state: State, resource: Resource, resourceState: Map[ID, SPValue]): RenderCard = {
      val relatedDrivers = resource.stateMap.map { case OneToOneMapper(_, driverId, _) => driverId}.distinct
      val selectedDrivers = state.theDrivers.filter { case (driver, _, _) => relatedDrivers.contains(driver.id) }
      val driverStatuses = selectedDrivers.map { case (driver, _, status) => (driver.name, status) }

      val resultState = resource.stateMap.map { case mapper: VD.OneToOneMapper =>
        (mapper.driverIdentifier.toString, resourceState(mapper.thing))
      }

      SPCardGrid.ResourceCard(
        cardId = resource.id,
        name = resource.name,
        driverStatuses = driverStatuses,
        state = resultState
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
