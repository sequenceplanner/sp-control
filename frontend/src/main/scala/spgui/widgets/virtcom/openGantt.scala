package spgui.widgets.virtcom

import sp.domain._
import spgui.circuit.{AddWidget, SPGUICircuit, UpdateWidgetData}
import spgui.widgets.ganttviewer.APIGantt.{APIGanttViewer => apiGantt}
import japgolly.scalajs.react.Callback

import scala.scalajs.js

object openGantt  {

  def showGantt(gantt : List[(ID, Double, Double)], ids : List[IDAble]) ={

    val ganttForViewer = gantt.map { op =>
      apiGantt.row(rowName = ids.find(_.id == op._1).get.name, eventName = "", startT= (new js.Date(op._2 * 1000)), endT= (new js.Date(op._3 * 1000)))}

    val newWidget = AddWidget("Gantt Viewer", 10, 5) // "Create" new widget
    SPGUICircuit.dispatch(UpdateWidgetData(newWidget.id, SPValue(apiGantt.openGantt(ganttForViewer)))) // Send initial data to widget
    SPGUICircuit.dispatch(newWidget) // start the new gantt viewer widget

    Callback.empty
  }
}
