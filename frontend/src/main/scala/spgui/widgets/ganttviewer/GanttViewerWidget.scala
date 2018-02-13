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
import spgui.widgets.ganttviewer.APIGantt.APIGanttViewer.{Response, openGantt, row, topicResponse}

// This widget displays static gantt charts. It can be initialized using UpdateWidgetData, for an example see openGantt in virtcom
// The widget data can also be set by sending messages via the API
// If you want to update an already opened gantt viewer with existing gantt data, the widget should be initialized with an ID used for data updates

// The gantt chart consists of rows, each row having a name.
// In these rows events can be placed with start and end times given in the js.Date format.
// Each of these events can be given a name
// To decide color of a row, simply set the row color attribute to a value such as "red", "#8f2525" etc

// The time format and view scale can take values such as "day", "hour", "second" and "day", "2 days", "1 minutes" etc
// Currently these values can not be updated once set.


object GanttViewerWidget {
  var initialData: SPValue = _
  private class Backend($: BackendScope[SPWidgetBase,Unit]) {
    var spGantt: SPGantt = _;  var divID  =ID.newID.toString
    val messObs = BackendCommunication.getMessageObserver(
      mess => {
        val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[Response].map {

          case openGantt(gantt, timeformat, viewScale, messDivID) =>
          if (spGantt == null && (dom.document.getElementById(divID) != null)) { // if spGantt has not been initialized do so and set the gantt data

            spGantt = SPGantt(dom.document.getElementById(divID), SPGanttOptions(headers = js.Array(timeformat), viewScale = viewScale))
            setGanttData(gantt)
          }
          else if( messDivID == divID && (dom.document.getElementById(divID) != null && spGantt != null)){ // if the message Div ID matches the id of the existing widget Div, then update the gantt data
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
      val ganttFormated = gantt.map { row => Row(row.rowName, js.Array(Task(row.eventName,row.startT, row.endT, row.color) ) )}.toJSArray
      spGantt.setData(ganttFormated)
    }

    def render() =
    <.div(
      ^.id := divID,
        HtmlTagOf[dom.html.Element]("gantt-component"), // becomes <gantt-component></gantt-component>
    )


    def onUnmount() = { // Unmounting widget
      println("Unmounting")
      messObs.kill()
      Callback.empty
    }
  }

  private val component = ScalaComponent.builder[SPWidgetBase]("GanttViewer")
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .componentDidMount(dcb => Callback{ // If the widget is open

      if (initialData != SPValue.empty) { // check if there is some initial data
        import spgui.widgets.ganttviewer.APIGantt.APIGanttViewer.Formats._
        initialData.asOpt[openGantt].foreach(ganttData => { // if the data is of the correct format
          dcb.backend.spGantt = SPGantt(dcb.getDOMNode, SPGanttOptions(headers = js.Array(ganttData.timeformat), viewScale = ganttData.viewScale)) // Set spGantt of widget

          import js.JSConverters._
          val ganttFormated = ganttData.gantt.map { row => Row(row.rowName, js.Array(Task(row.eventName, row.startT, row.endT, row.color))) }.toJSArray // Convert data to javascript
          dcb.backend.spGantt.setData(ganttFormated) // set initial gantt data

          dcb.backend.divID = ganttData.messDivID.toString // update the div ID
        })}
    })
    .build

  def apply() = SPWidget(spwb => {initialData = spwb.getWidgetData; component(spwb);})

}
