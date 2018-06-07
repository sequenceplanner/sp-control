package spgui

import sp.PersistentGUIState
import sp.domain.{SPHeader, SPMessage}
import spgui.circuit.{DashboardPresets, SPGUICircuit}
import spgui.communication.BackendCommunication
import spgui.dashboard.{DashboardPresetsComponent, DashboardPresetsMenu}
import spgui.menu.SPMenu

/**
  * Sets up and connects the preset menu, and emits DashboardPreset events to the backend.
  */
class DashboardComponentThing extends DashboardPresetsComponent {
  import spgui.circuit.DashboardPresets.Formats._

  /**
    * Adds a layout preset menu to the SP header menu
    */
  def config = DashboardPresetsMenu.Config
    .onMount(() => emit(DashboardPresets.MenuMounted))
    .onLoad(p => emit(DashboardPresets.LoadPreset(p)))
    .onSave(p => emit(DashboardPresets.SavePreset(p)))
    .onDelete(p => emit(DashboardPresets.DeletePreset(p)))
    .onUpdate(props => emit(Wrapper(props.proxy.value.presets.toList)))
}
