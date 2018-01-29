package spgui.widgets.ganttviewer

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scalajs.js
import org.scalajs.dom
import spgui.SPWidget
import spgui.communication.BackendCommunication
import play.api.libs.json._
import sp.domain._
import spgui.widgets.gantt._
import spgui.SPWidgetBase
import sp.domain.logic.JsonImplicit
import sp.domain.Logic._
import spgui.widgets.ganttviewer.APIGanttViewer.row


object APIGanttViewer {
  sealed trait Request
  sealed trait Response
  val topicRequest = "GanttViewerRequests"
  val topicResponse = "GanttViewerResponse"

  case class row(rowName: String, eventName: String, startT: Double, endT: Double)
  case class openGantt(gantt : List[row], timeformat : String = "second", viewScale : String = "1 seconds", updateGantt : Boolean = false) extends Response

  object Formats {
    implicit val frow: JSFormat[row] = Json.format[row]
    implicit val fopenGantt: JSFormat[openGantt] = Json.format[openGantt]

    def fGanttViewerRequest: JSFormat[Request] = Json.format[Request]
    def fGanttViewerResponse: JSFormat[Response] = Json.format[Response]
  }

  object Request {
    implicit lazy val fGanttViewerRequest: JSFormat[Request] = Formats.fGanttViewerRequest
  }
  object Response {
    implicit lazy val fGanttViewerResponse: JSFormat[Response] = Formats.fGanttViewerResponse
  }
}


object GanttViewerWidget {

  private class Backend($: BackendScope[SPWidgetBase,Unit]) {
    var spGantt: SPGantt = _;  var divID =""
    val messObs = BackendCommunication.getMessageObserver(
      mess => {
        val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIGanttViewer.Response].map {
          case APIGanttViewer.openGantt(gantt, timeformat, viewScale, updateGantt) =>
          if (spGantt == null && (dom.document.getElementById(divID) != null)) {

            spGantt = SPGantt(dom.document.getElementById(divID), SPGanttOptions(headers = js.Array(timeformat), viewScale = viewScale))
            setGanttData(gantt)
          }
          else if(updateGantt && (dom.document.getElementById(divID) != null && spGantt != null)){
            setGanttData(gantt)
          }
          Callback.empty
          case x => Callback.empty
        }
        callback.foreach(_.runNow())
      },
      APIGanttViewer.topicResponse
    )

    def setGanttData (gantt : List[row]) ={
      import js.JSConverters._
      val ganttFormated = gantt.map { row => Row(row.rowName, js.Array(Task(row.eventName,new js.Date(row.startT), new js.Date(row.endT))) )}.toJSArray
      spGantt.setData(ganttFormated)
    }

    def render() =

    <.div(
      ^.id := newID,
        HtmlTagOf[dom.html.Element]("gantt-component"), // becomes <gantt-component></gantt-component>
    )
    def newID ={if(divID == "") {divID =ID.newID.toString}; divID}


    def onUnmount() = { // Unmounting widget
      println("Unmounting")
      messObs.kill()
      Callback.empty
    }
  }

  private val component = ScalaComponent.builder[SPWidgetBase]("GanttViewer")
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build
  def apply() = SPWidget(spwb => component(spwb))
}
