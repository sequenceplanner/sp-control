package spgui.widgets.virtcom

import sp.domain.{ID, IDAble}
import spgui.circuit.{AddWidget, SPGUICircuit}
import spgui.widgets.virtcom.sendMessages.sendToGanttViewer
import spgui.widgets.ganttviewer.APIGantt.{APIGanttViewer => apiGantt}

object openGantt {

  def showGantt(gantt : List[(ID, Double, Double)], ids : List[IDAble]) ={
    SPGUICircuit.dispatch(AddWidget("Gantt Viewer", 10, 5)) // Todo: try to make sure that the widget is open before sending data to it.

    val ganttForViewer = gantt.map { op =>
      apiGantt.row(rowName = ids.find(_.id == op._1).get.name, eventName = "", startT= (op._2 * 1000), endT= (op._3 * 1000))}

    sendToGanttViewer(apiGantt.openGantt(ganttForViewer))
  }
}
