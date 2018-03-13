package spgui.widgets.vdtesting

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import spgui.circuit.{SPGUICircuit, SetTheme}

import spgui.SPWidget
import spgui.components.Icon
import spgui.components.{SPWidgetElements => Comp}
import spgui.communication._
import sp.domain.SPAttributes._
import sp.domain.SPMessage
import sp.domain.Logic._
import sp.domain.SPValue
import scalajs.js._
import sp.domain._

object VDTracker {
  //import sp.devicehandler._
  import sp.devicehandler.APIDeviceDriver
  import sp.runners.APIOperationRunner
  import sp.abilityhandler.APIAbilityHandler
  import sp.devicehandler.APIVirtualDevice

  case class State(currentDriverState: Map[String, SPValue] = Map(),
                   latestEvent: Map[ID, SPValue] = Map(),
                   latestAbilityState: Map[ID, SPValue] = Map(),
                   latestVDeviceState: Map[ID, SPValue] = Map()
  )


  private class Backend($: BackendScope[Unit, State]) {
    // val vdObs = BackendCommunication.getWebSocketStatusObserver(  mess => {
    //   //if (mess) sendToVD(vdapi.GetVD)
    // }, vdapi.topicResponse)

    val deviceDriverHandler =
      BackendCommunication.getMessageObserver(onDeviceDriverMessage, APIDeviceDriver.topicResponse)
    val operationRunnerHandler =
      BackendCommunication.getMessageObserver(onOperationRunnerMessage, APIOperationRunner.topicResponse)
    val abilityHandler =
      BackendCommunication.getMessageObserver(onAbilityMessage, APIAbilityHandler.topicResponse)
    val virtualDeviceHandler = 
      BackendCommunication.getMessageObserver(onVirtualDeviceMessage, APIVirtualDevice.topicResponse)

    def onDeviceDriverMessage(mess: SPMessage): Unit = {
      mess.body.to[APIDeviceDriver.Response].map{
        case mess:APIDeviceDriver.DriverStateChange => {
          onDriverStateChange(mess)
        }
        case _ => println("uhh")
      }
    }

    def onOperationRunnerMessage(mess: SPMessage): Unit = {
      mess.body.to[APIOperationRunner.Response].map{
        case APIOperationRunner.StateEvent(runnerID, state) => {
          $.modState(s => s.copy(latestEvent = state)).runNow()
        }
      }

    }
    def onAbilityMessage(mess: SPMessage): Unit = {
      mess.body.to[APIAbilityHandler.Response].map{
        case APIAbilityHandler.AbilityState(id, state) => {
           $.modState(s => s.copy(latestAbilityState = state)).runNow()
        }
      }
    }
    def onVirtualDeviceMessage(mess: SPMessage): Unit = {
      mess.body.to[APIVirtualDevice.Response].map{
        case APIVirtualDevice.StateEvent(resource, id, state, diff) => {
          $.modState(s => s.copy(latestVDeviceState = state)).runNow()
        }
      }
    }


    def render(p:Unit, s:State) =
      <.div(
        <.div("current driver state: ", s.currentDriverState.toString),
        <.div(),
        <.div("latest event:", s.latestEvent.toString),
        <.div(),
        <.div("latest ability state: ", s.latestAbilityState.toString),
        <.div(),
        <.div("latest VDevice state:", s.latestVDeviceState.toString)
      )

    def onDriverStateChange(mess: APIDeviceDriver.DriverStateChange) {
      $.modState(s => s.copy(currentDriverState = mess.state)).runNow()
    }

    def onUnmount(): Callback =  {
      deviceDriverHandler.kill
      Callback.empty
    }
  }

  private val component = ScalaComponent.builder[Unit]("VDTracker")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = SPWidget(swpb => component())
}



// current driver state: Map(currentPos -> 60, refPos -> 60, active -> true, hasTool -> false)


// latest event:Map(
//  63b81bfd-5c1f-406e-a29f-413890e6045b -> "i",
//  b942d6a2-ca3f-47d0-a3e8-a8cf09cff21e -> "empty", 
//  7e5b4c01-3c8a-4fd0-9f99-2ecd6258e105 -> "f", 
//  f13dbfd5-93aa-4fd2-8233-05526dd5d6be -> "i", 
//  cfaa83b3-6785-4172-af91-73d3bfbe01a7 -> "i", 
//  02a84fc4-a12c-460e-af5a-091e87569487 -> 60, 
//  71181ec5-be34-4d4c-bdb6-17c7821b7ba1 -> "i", 
//  f54fc77e-a570-4de8-b490-f0db4a0baadb -> "f", 
//  ad91144a-639e-4877-90bf-7ff67f45fed8 -> 60, 
//  317dedf0-6791-482e-a52e-8bdb387772e9 -> "f", 
//  4bdfc4e2-f2d6-4077-a4da-28dd15f7c0a7 -> "i", 
//  fb424d78-eebc-48f8-b990-7fdc4d09089c -> "i", 
//  6974a19f-d498-435b-9b55-2379f31cce15 -> false, 
//  dde4072e-dc23-446a-be56-059e90b318b5 -> "empty", 
//  3e56ef17-77f8-4103-98aa-c5f712d66782 -> "f", 
//  62074c1c-dfce-4324-b0c2-546d0f4d302d -> 60, 
//  a62c39bb-568d-44d3-b07c-271fb00b5ba1 -> 60, 
//  4eb9b229-9823-44ab-b953-0621b21d7ed6 -> "empty", 
//  44b6d57d-7193-483b-acc6-582aae64a8e6 -> "f", 
//  843bf16c-881e-42a6-8005-6f72ccae92ab -> "enabled", 
//  10ce5906-ecdd-4d44-b339-3fac7cbde200 -> "empty", 
//  b498f684-04b5-4463-b90f-b781e0014c6b -> "notEnabled", 
//  c50b29da-a5e3-4da4-8c1e-f9624403c6ac -> "notEnabled", 
//  1010590c-3999-4444-a9db-05ee7d6d7237 -> false, 
//  e39419ac-c23b-40a5-998f-9eb19a83141c -> "empty", 
//  9df5375d-e38d-41ac-bd10-2d52d23e4cca -> "enabled", 
//  6ee53ae7-6e4f-4b6d-9db3-7debea5d598d -> "empty", 
//  67d4d1bf-3a18-4b45-b781-5cd4beb79c78 -> false, 
//  a34e59d9-f23c-4e9b-99af-296ed35c5c94 -> "empty", 
//  4728660a-645e-4736-ba32-3fa5687c8e6a -> "i", 
//  91f5d639-61f3-4673-9cc2-75868ce538d8 -> false, 
//  0c636ae5-6593-4c9e-b47f-b00549cea926 -> "f")


// latest ability state: Map(843bf16c-881e-42a6-8005-6f72ccae92ab -> {"state":"enabled","counter":0})


// latest VDevice state:Map(38a8ab3a-3c76-434a-9c82-11ee5d4b5333 -> 60, 6cdd446d-30a1-4304-b536-dc199a11572b -> true, 6b57c9ec-5944-4282-a6a7-ad43f0124ac4 -> 60)
