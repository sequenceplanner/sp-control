package sp

import play.api.libs.json._
import sp.domain._
import sp.domain.Logic._


object APIDashboardPresets {
  sealed trait Event

  case object DashboardPresetsRequest extends Event
  case class AllDashboardPresets(presets: Map[String, String]) extends Event
  case class AddDashboardPreset(name: String, preset: String) extends Event
  case class RemoveDashboardPreset(name: String) extends Event

  val DashboardPresetsTopic = "dashboard-presets"

  object Formats {
    implicit val fDashboardPresetsRequest: JSFormat[DashboardPresetsRequest.type] = deriveCaseObject[DashboardPresetsRequest.type]
    implicit val fDashboardPresetsReply: JSFormat[AllDashboardPresets] = Json.format[AllDashboardPresets]
    implicit val fAddDashboardPreset: JSFormat[AddDashboardPreset] = Json.format[AddDashboardPreset]
    implicit val fRemoveDashboardPreset: JSFormat[RemoveDashboardPreset] = Json.format[RemoveDashboardPreset]
    def fAPI_Event: JSFormat[Event] = Json.format[Event]
  }

  object Event {
    implicit lazy val fAPI_Event: JSFormat[Event] = Formats.fAPI_Event
  }
}