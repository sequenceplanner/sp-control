package spgui.widgets.ganttviewer

import play.api.libs.json._
import sp.domain._
import sp.domain.logic.JsonImplicit
import sp.domain.Logic._

object APIGantt {

  object APIGanttViewer {
    sealed trait Response
    val topicResponse = "GanttViewerResponse"

    case class row(rowName: String, eventName: String, startT: Double, endT: Double)
    case class openGantt(gantt : List[row], timeformat : String = "second", viewScale : String = "1 seconds", updateGantt : Boolean = false) extends Response

    //case class openG(gantt : js.Array[spgui.widgets.gantt.Row], timeformat : String = "second", viewScale : String = "1 seconds", updateGantt : Boolean = false) extends Response
    object Formats {
      implicit val frow: JSFormat[row] = Json.format[row]
      implicit val fopenGantt: JSFormat[openGantt] = Json.format[openGantt]
      //implicit val fopenG: JSFormat[openG] = Json.format[openG]
      def fGanttViewerResponse: JSFormat[Response] = Json.format[Response]
    }
    object Response {
      implicit lazy val fGanttViewerResponse: JSFormat[Response] = Formats.fGanttViewerResponse
    }
  }
}
