package spgui.widgets.ganttviewer

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._

import scalajs.js
import org.scalajs.dom
import spgui.SPWidget
import spgui.communication.BackendCommunication
import sp.domain._
import spgui.widgets.gantt._
import spgui.SPWidgetBase
import spgui.widgets.ganttviewer.APIGantt.APIGanttViewer.{row,openGantt,Response, topicResponse}


object GanttViewerWidget {

  private class Backend($: BackendScope[SPWidgetBase,Unit]) {
    var spGantt: SPGantt = _;  var divID =""
    val messObs = BackendCommunication.getMessageObserver(
      mess => {
        val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[Response].map {
          case openGantt(gantt, timeformat, viewScale, updateGantt) =>
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
      topicResponse
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
