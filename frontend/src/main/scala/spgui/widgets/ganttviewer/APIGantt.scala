package spgui.widgets.ganttviewer

import play.api.libs.json._
import sp.domain._
import sp.domain.logic.JsonImplicit
import sp.domain.Logic._

import scala.scalajs.js

object APIGantt {

  object APIGanttViewer {
    sealed trait Response
    val topicResponse = "GanttViewerResponse"

    case class row(rowName: String, eventName: String, startT: js.Date, endT: js.Date, color: String = "grey")
    case class openGantt(gantt : List[row], timeformat : String = "second", viewScale : String = "1 seconds", messDivID : ID = ID.newID) extends Response

    object Formats {
    // read and write for java script Date
      implicit lazy val readDate: Reads[js.Date] = new Reads[js.Date] {
        def reads(json: JsValue): JsResult[js.Date] = {
          val rr = (JsPath \ "d").read[String].map(d => new js.Date(d))
          extrObj(json, "Date", rr)
        }
      }
      implicit lazy val writeDate: Writes[js.Date] = new Writes[js.Date] {
        override def writes(d: js.Date): SPValue =  Json.obj(
          "isa"->"Date",
          "d" -> d.toJSON()
        )
      }

      implicit val fRow: JSFormat[row] = Json.format[row]
      implicit val fOpenGantt: JSFormat[openGantt] = Json.format[openGantt]

      def fGanttViewerResponse: JSFormat[Response] = Json.format[Response]
    }
    object Response {
      implicit lazy val fGanttViewerResponse: JSFormat[Response] = Formats.fGanttViewerResponse
    }
  }
}
