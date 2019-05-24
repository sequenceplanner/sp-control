package spgui

import japgolly.scalajs.react.vdom.html_<^.VdomElement

object Widgets {

  type Widget = (String, SPWidgetBase => VdomElement, Int, Int)

  val sp =
    List[Widget](
//    ("Grid Test",                   spgui.dashboard.GridTest(),                    5, 5),
//    ("Widget Injection",            widgets.injection.WidgetInjectionTest(),       3, 4),
//    ("DragDrop Example",            widgets.examples.DragAndDrop(),                3, 4),
//    ("Widget with json",            widgets.examples.WidgetWithJSON(),             3, 4),
//    ("PlcHldrC",                    PlaceholderComp(),                             3, 4),
//    ("SPWBTest",                    SPWidgetBaseTest(),                            3, 4),
//    ("Widget with data",            widgets.examples.WidgetWithData(),             3, 4),
//    ("D3Example",                   widgets.examples.D3Example(),                  3, 4),
//    ("D3ExampleServiceWidget",      widgets.examples.D3ExampleServiceWidget(),     3, 4),
//    ("ExampleServiceWidget",        widgets.examples.ExampleServiceWidget(),                        3, 4),
//    ("ExampleServiceWidgetState",   widgets.examples.ExampleServiceWidgetState(),                   3, 3),
//    ("LabkitExperimentWidget",      widgets.labkit.LabkitExperimentWidget(),            3, 4),
//    ("Item explorer",               widgets.itemexplorer.ItemExplorer(),                3, 4),
//    ("Live Gantt Example",          widgets.gantt.LiveGanttExample(),                   10, 5),

//      ("OpcUAWidget",                 widgets.examples.OpcUAWidget(),                     5, 4),
      ("Item explorer",    widgets.itemexplorerincontrol.ItemExplorer(),                  3, 4),
//      ("Item explorer tree",          widgets.itemtree.ItemExplorer(),                    2, 4),
      ("Gantt Viewer",                widgets.ganttviewer.GanttViewerWidget(),            10, 5),
      ("ServiceList",                 widgets.services.ServiceListWidget(),               3, 4),
      ("SopMaker",                    widgets.sopmaker.SopMakerWidget(),                  3, 4),
      ("SopRunner",                   widgets.sopmaker.SopRunnerWidget(),                 3, 4),
      ("ModelsWidget",                widgets.model.ModelsWidget(),                       3, 6),
      ("Item Editor",                 widgets.itemeditorincontrol.ItemEditorInControl(),  3, 4),
      ("DummyLiveGantt",              widgets.ganttviewer.DummyLiveGantt(),               10, 5),
      ("SPModelImportWidget",         widgets.modelImport.SPModelImportWidget(),          5, 10),
      ("MiniModel helper",                  widgets.MiniModelHelper.MiniModelHelperWidget(),        5, 5),
      ("HumanInstructions",           widgets.unification.HumanInstructionsWidget(),      5, 5),
      ("RunnerState",          widgets.examples.RunnerStateWidgetState(),          6, 6),
      ("Restart",          widgets.restart.RestartWidget(),          4, 5),
      ("ROS2Widget",          widgets.examples.ROS2Widget(),          4, 5),
      ("URDemo",          widgets.examples.URDemoWidget(),          4, 5),



    )


  def loadWidgets(): Unit = {
    WidgetList.addWidgets(sp)
  }
}
