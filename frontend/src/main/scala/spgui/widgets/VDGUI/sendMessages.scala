package spgui.widgets.VDGUI

import japgolly.scalajs.react.Callback
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.{APIDeviceDriver, APIVirtualDevice, VD}
import sp.domain.{ID, SPHeader, SPMessage, SPValue}
import sp.vdtesting.APIVDTracker
import sp.models.{APIModel => mapi}
import sp.runners.APIOperationRunner
import spgui.communication.BackendCommunication
import spgui.widgets.VDGUI.VDTracker.State

object sendMessages {
  def sendToDeviceDriver(mess: APIDeviceDriver.Request) = Callback{
    val h = SPHeader(from = "DriverWidget", to = "DriverService", reply = SPValue("DriverWidget"))
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, APIDeviceDriver.topicRequest)
  }

  def sendToRunner(mess: APIOperationRunner.Request) = Callback{
    val h = SPHeader(from = "UI", to = APIOperationRunner.service)
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, APIOperationRunner.topicRequest)
  }

  def sendToModel(model: ID, mess: mapi.Request)= Callback{ //  Send message to model
    val h = SPHeader(from = "VDTrackerWidget", to = model.toString,
      reply = SPValue("VDTrackerWidget"))
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, mapi.topicRequest)
  }

  def sendToVDTrackerService(mess: APIVDTracker.Request) =  Callback {
    val h = SPHeader(from = "VDTrackerWidget", to = APIVDTracker.service, reply = SPValue("VDTracker"))
    val json = SPMessage.make(h, mess)
    BackendCommunication.publish(json, APIVDTracker.topicRequest)
  }

  def resetVDGUI(s :State) = {
    val h = SPHeader(from = "VDTrackerWidget")
    val json = SPMessage.make(h, APIVDTracker.ResetGUI)
    BackendCommunication.publish(json, APIVDTracker.topicRequest)
  }

  def resetDriverState = {
    val h = SPHeader(from = "VDTrackerWidget", to = APIVirtualDevice.service)
    val json = SPMessage.make(h, APIVirtualDevice.TerminateAllVDs)
    BackendCommunication.publish(json, APIVirtualDevice.topicRequest)
  }

  def terminateVDs = {
    val h = SPHeader(from = "VDTrackerWidget", to = APIVirtualDevice.service)
    val json = SPMessage.make(h, APIVirtualDevice.TerminateAllVDs)
    BackendCommunication.publish(json, APIVirtualDevice.topicRequest)
  }

  def terminateAbilities = {
    val h = SPHeader(from = "VDTrackerWidget", to = APIAbilityHandler.service)
    val json = SPMessage.make(h, APIAbilityHandler.TerminateAllAbilities)
    BackendCommunication.publish(json, APIAbilityHandler.topicRequest)
  }

  def terminateDrivers(drivers : List[VD.Driver]) = {
    val h = SPHeader(from = "VDTrackerWidget", to = "DriverService")
    drivers.map(d => { val json = SPMessage.make(h, APIDeviceDriver.TerminateDriver(d.id))
      BackendCommunication.publish(json, APIDeviceDriver.topicRequest) })
  }

  def terminateRunners(rs : Map[ID, Map[ID,SPValue]]) = {
    val h = SPHeader(from = "VDTrackerWidget", to = APIOperationRunner.service)
    rs.keys.map(r => { val json = SPMessage.make(h, APIOperationRunner.TerminateRunner(r))
      BackendCommunication.publish(json, APIOperationRunner.topicRequest) })
  }

  def sendTerminateRunner(id: ID) = {
    val h = SPHeader(from = "VDTrackerWidget", to = APIOperationRunner.service)
    val json = SPMessage.make(h, APIOperationRunner.TerminateRunner(id))
    BackendCommunication.publish(json, APIOperationRunner.topicRequest)
  }


}
