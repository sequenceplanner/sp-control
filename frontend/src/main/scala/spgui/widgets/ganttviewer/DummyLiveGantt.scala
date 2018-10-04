package spgui.widgets.ganttviewer

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scalajs.js
import scalajs.js.JSConverters._
import sp.domain._
import sp.runners.{APIOperationRunner => oprapi}
import spgui.{SPWidget, SPWidgetBase}
import spgui.communication.APIComm.StreamHelper
import spgui.widgets.gantt.{Row, SPGantt, SPGanttOptions, Task}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object DummyLiveGantt {

  // TODO runnerID should possibly be props
  case class State(
                    runnerID: ID = null,
                    abilityNames: Map[ID, String] = Map(), // operation ID -> ability name
                    oprState: Map[ID, String] = Map() // operation ID -> "i", "e" or "f"
                  )

  private class Backend($: BackendScope[SPWidgetBase, State]) {

    val oprComm = new OperationRunnerAPIComm(
      stateEvent => {
        $.modState {
          s => s.copy(
            runnerID = stateEvent.runnerID,
            oprState = s.oprState ++ stateEvent.state.flatMap { kv =>
              kv._2.asOpt[String] match {
                case Some("i") => Some(kv._1 -> "i")
                case Some("e") => Some(kv._1 -> "e")
                case Some("f") => Some(kv._1 -> "f")
                case _ => None
              }
            }
          )
        }
      }.runNow()
    )

    def getRunnerSetup(runnerID: ID) = {
      oprComm.request(oprapi.GetRunner(runnerID)).takeFirstResponse.map(_._2).collect { case oprapi.Runner(setup) => setup}
    }

    def render(s: State) = {
      <.div(
        "Open VDTracker -> Create model \"DummyExample\" -> click both Launch buttons to get data",
        /*
        <.button("getRunner", ^.onClick --> getAbilityNames(s.runnerID)),
        <.div(s.abilityNames.mkString),
        <.ul(
          s.oprState.toTagMod { case (id, state) =>
            <.li(s.abilityNames.get(id).getOrElse(id.toString) + " state: ", state)
          }
        ),
        */
        DummyGanttComponent(s.abilityNames, s.oprState)
      )
    }

  }

  private val component = ScalaComponent.builder[SPWidgetBase]("DummyLiveGantt")
    .initialState(State())
    .renderBackend[Backend]
    .componentDidUpdate { ctx =>
      val runnerID = ctx.currentState.runnerID
      // if (ctx.prevState.runnerID == null && runnerID != null) ctx.backend.getAbilityNames(runnerID)
      // else
        Callback.empty
    }
    .build

  def apply() = SPWidget(spwb => component(spwb))

}

object DummyGanttComponent {

  case class Props(abilityNames: Map[ID, String], oprState: Map[ID, String])
  case class State(
                    rows: Map[ID, (Boolean, Row)] = Map(), // operation ID -> (isActive, gantt-Row)
                    startTime: Option[js.Date] = None
                  )

  class Backend($: BackendScope[Props, State]) {

    var spGantt: SPGantt = _

    def render(p: Props) = {
      <.div(
        HtmlTagOf[dom.html.Element]("gantt-component") // becomes <gantt-component></gantt-component>
      )
    }
  }

  private val component = ScalaComponent.builder[Props]("DummyGanttComponent")
    /*
    .initialStateFromProps { p =>
      val rows = p.abilityNames.map { case (id, name) => id -> (false, Row(name, js.Array())) }
      State(rows)
    }
    */
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(ctx => Callback {
      ctx.backend.spGantt = SPGantt(ctx.getDOMNode, SPGanttOptions(headers = js.Array("second"), viewScale = "1 seconds"))
    })
    .componentWillReceiveProps { ctx =>
      val now = new js.Date()
      val startTime = ctx.state.startTime.getOrElse(now)
      val nextRows = ctx.nextProps.oprState.map { case (id, oprStateStr) =>
        val name = ctx.nextProps.abilityNames.getOrElse(id, id.toString)
        val currentIsActive = ctx.state.rows.get(id).map(_._1).getOrElse(false)
        val currentTasksOp = ctx.state.rows.get(id).map(_._2.tasks)
        val currentTasks = currentTasksOp.getOrElse(js.Array())
        val (nextIsActive, nextTasks): (Boolean, js.Array[Task]) = (currentIsActive, oprStateStr) match {
          case (_, "e") if currentTasks.isEmpty => (true, js.Array(Task(name, now, now)))
          case (false, "e") => (true, currentTasks :+ Task(name, now, now))
          case (true, "e") => (true, currentTasks.init :+ Task(name, currentTasks.last.from, now))
          case (_, "f") => (false, currentTasks)
          case (_, "i") => (false, currentTasks)
          case _ => (false, currentTasks)
        }
        id -> (nextIsActive, Row(name, nextTasks))
      }
      ctx.setState(State(nextRows, Some(startTime)))
    }
    .componentDidUpdate(ctx => Callback {
      ctx.backend.spGantt.setData(ctx.currentState.rows.values.map(_._2).toJSArray)
    })
    .build

  def apply(abilityNames: Map[ID, String], oprState: Map[ID, String]) =
    component(Props(abilityNames, oprState))
}
