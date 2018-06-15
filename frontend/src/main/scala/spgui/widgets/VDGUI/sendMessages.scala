package spgui.widgets.VDGUI

import japgolly.scalajs.react.Callback
import sp.devicehandler.APIDeviceDriver
import sp.domain.{ID, SPHeader, SPMessage, SPValue}
import spgui.communication.BackendCommunication

object sendMessages {
  def sendToDeviceDriver(mess: APIDeviceDriver.Request) = Callback{
    val h = SPHeader(from = "DriverWidget", to = "DriverService", reply = SPValue("DriverWidget"))
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, APIDeviceDriver.topicRequest)
  }

}
