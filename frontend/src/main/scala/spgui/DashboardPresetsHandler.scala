package spgui

import sp.domain._
import sp.{APIDashboardPresets => API}
import spgui.circuit.DashboardPreset
import spgui.dashboard.AbstractDashboardPresetsHandler

/**
  * Created by alfredbjork on 2018-04-06.
  */
class DashboardPresetsHandler extends AbstractDashboardPresetsHandler {
  private val me = "DashboardPresetsHandler"

  override protected def dashboardPresetsTopic: String = API.DashboardPresetsTopic

  override def handleMsg(msg: SPMessage): Unit = {
    import API.Formats._
    import SPHeader._

    for{
      header <- msg.getHeaderAs[SPHeader]
      event <- msg.getBodyAs[API.AllDashboardPresets]
    } handleEvent(event, header.to)
  }

  private def handleEvent(event: API.AllDashboardPresets, to: String) = {
    if (to == me){
      println("Received " + event.presets.size + " presets")
      updateGUIState(event.presets.mapValues(s => fromJson(s)))
    }
  }

  override def requestPresets(): Unit = {
    // Onloadsite
    println("Asking for presets")
    sendMsg[API.DashboardPresetsRequest.type](from = me, to = "PersistentStorage", payload = API.DashboardPresetsRequest)
  }

  override def storePresetToPersistentStorage(name: String, preset: DashboardPreset): Unit = {
    //onSave
    println("Storing preset")
    sendMsg[API.AddDashboardPreset](from = me, to = "PersistentStorage", payload = API.AddDashboardPreset(name, toJson(preset)))
  }

  override def removePresetFromPersistentStorage(name: String): Unit = {
    // Remove preset
    println("Removing preset")
    sendMsg[API.RemoveDashboardPreset](from = me, to = "PersistentStorage", payload = API.RemoveDashboardPreset(name))
  }
}

object DashboardPresets {
  val handler = new DashboardPresetsHandler()
}
