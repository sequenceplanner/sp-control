package spgui.widgets.ganttviewer

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom

import scalajs.js
import scalajs.js.JSConverters._
import sp.domain._
import sp.domain.Logic._

import spgui.{SPWidget, SPWidgetBase}
import spgui.communication.APIComm.StreamHelper
import spgui.widgets.gantt.{Row, SPGantt, SPGanttOptions, Task}


import diode.react.{ModelProxy, ReactConnectProxy}
import spgui.communication._
import spgui.circuits.main.MainCircuit
import spgui.circuits.main.handlers.ModelHandlerState


import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object DummyLiveGantt {

  case class State(state: Map[ID, SPValue] = Map(), ops: Map[ID, Operation] = Map())
  case class Props(proxy: ModelProxy[ModelHandlerState])

  private class Backend($: BackendScope[Props, State]) {

    val oprComm = new OperationRunnerAPIComm(
      stateEvent => {
        $.modState {
          s => s.copy(
            state = s.state ++ stateEvent.state
          )
        }
      }.runNow()
    )

    def onReceiveProps(props: Props) = {
      $.modState(state => {
        props.proxy.value.activeModel.map{ model =>
          val l = model.items.toList  /// cannot collect on the simpleset.. crashes. figure out at a later date
          val o = l.collect{case o: Operation => o.id -> o}.toMap
          state.copy(ops = o)
        }.getOrElse(state)
      })
    }

    def render(p: Props, s: State) = {
      <.div(
        DummyGanttComponent(s.state, s.ops)
      )
    }

  }

  private val component = ScalaComponent.builder[Props]("DummyLiveGantt")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillReceiveProps{
      scope => scope.backend.onReceiveProps(scope.nextProps)
    }
    .build

  val connectCircuit: ReactConnectProxy[ModelHandlerState] = MainCircuit.connect(_.models)

  def apply() = spgui.SPWidget(_ => connectCircuit { proxy => component(Props(proxy)) })
}

object DummyGanttComponent {

  case class Props(state: Map[ID, SPValue], ops: Map[ID, Operation])
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
      val nextRows = for {
        (id, opstate) <- ctx.nextProps.state
        name <- ctx.nextProps.ops.get(id).map(_.name)
      } yield {
        val currentIsActive = ctx.state.rows.get(id).map(_._1).getOrElse(false)
        val currentTasksOp = ctx.state.rows.get(id).map(_._2.tasks)
        val currentTasks = currentTasksOp.getOrElse(js.Array())
        val (nextIsActive, nextTasks): (Boolean, js.Array[Task]) = (currentIsActive, opstate.toString) match {
          case (_, "\"executing\"") if currentTasks.isEmpty => (true, js.Array(Task(name, now, now)))
          case (false, "\"executing\"") => (true, currentTasks :+ Task(name, now, now))
          case (true, "\"executing\"") => (true, currentTasks.init :+ Task(name, currentTasks.last.from, now))
          case (_, "\"finished\"") => (false, currentTasks)
          case (_, "\"enabled\"") => (false, currentTasks)
          case (_, "\"notEnabled\"") => (false, currentTasks)
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

  def apply(state: Map[ID, SPValue], ops: Map[ID, Operation]) =
    component(Props(state, ops))
}
