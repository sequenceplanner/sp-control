package spgui.widgets.VDGUI

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.{VD}

import sp.VDAggregator.APIVDAggregator
import sp.domain._
import spgui.communication._

/** Widget to visualize the Resources and it's status*/
object ResourceWidget {
  case class State(
                    resources: List[VD.ResourceWithState] = List(),
                    theDrivers: List[(VD.Driver, VD.DriverState, String)] = List()
                  )

  private class Backend($: BackendScope[Unit, State]) {

    val messHandler = BackendCommunication.getMessageObserver(onMessage, APIVDAggregator.topicResponse)

    /** Handle APIDeviceDriver-messages.
      *
      * If a [[APIVDAggregator.TheDrivers]] response is noticed,
      * update the local list of driver.
      *
      *
      * @param mess SPMessage from APIDeviceDriver
      */
    def onMessage(mess: SPMessage): Unit = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIVDAggregator.Response].map {
        case APIVDAggregator.TheDrivers(drivers) =>
          $.modState { s =>
            s.copy(theDrivers = drivers.map(d => (d.driver, d.driverState, d.status)))
          }
          case APIVDAggregator.TheResources(resources) =>
          $.modState { s: State => s.copy(resources = s.resources ++ resources)}

        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }


    /** Set SPHeader and send SPMessage to APIVDAggregator
      *
      * @param mess SPMessage of APIVDAggregator
      */
    def sendToAggregator(mess: APIVDAggregator.Request): Unit = {
      val header = SPHeader(from = "ResourceWidget", to = APIVDAggregator.service, reply = SPValue("ResourceWidget"))
      BackendCommunication.publish(SPMessage.make(header, mess), APIVDAggregator.topicRequest)
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
      messHandler.kill()
    }

    /** When the widget did mount, try to get all VD that is active.
      *
      * @return Callback
      */
    def didMount: Callback = Callback{
      println(s"Resource Widget didMount and is requesting GetResources")
      sendToAggregator(APIVDAggregator.GetResources)
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
