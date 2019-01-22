package spgui.widgets.examples

import diode.react.{ModelProxy, ReactConnectProxy}
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import spgui.SPWidgetBase
import spgui.components.SPWidgetElements
import spgui.communication._
import sp.domain._
import Logic._
import spgui.circuits.main.{FrontendState, MainCircuit}
import sp.runners.{APIRunnerManager => api}
import spgui.circuits.main.handlers._

case class Props(proxy: ModelProxy[FrontendState]) {
  val activeModel: Option[ModelMock] = proxy.value.models.activeModel
  val activeRunner: Option[ID] = proxy.value.runners.latestActiveRunnerId
  val runnerStates: Map[ID, Map[ID, SPValue]] = proxy.value.runners.runnerStates
}

object RunnerStateWidgetState {
  import spgui.widgets.examples.{RunnerStateCSS => css}

  val typeFilters = List("type", "operation", "internal", "input", "output")

  case class State(force: Map[ID, SPValue], activeForce: Set[ID], forceEvents: Map[ID, SPValue], idableFilter: String = "", typeFilter: String = typeFilters.head)

  private class Backend($: BackendScope[Props, State]) {

    def setForce(runnerID: ID, id: ID, value: SPValue): Callback = {
      $.modState { s =>
        val newForce = s.force + (id -> value)
        // if force active, send new force table to backend
        val f = newForce.filterKeys(s.activeForce.contains)
        send(api.SetForceTable(runnerID, f, s.forceEvents))
        s.copy(force = newForce)
      }
    }

    def setEvent(runnerID: ID, id: ID, value: SPValue): Callback = {
      $.modState { s =>
        val forceEvents =
          if(value == SPValue("[none]")) s.forceEvents - id
          else s.forceEvents + (id -> SPValue(value))

        val f = s.force.filterKeys(s.activeForce.contains)
        send(api.SetForceTable(runnerID, f, forceEvents))
        s.copy(forceEvents = forceEvents)
      }
    }


    def itemType(item: IDAble): String = {
      val input = item.attributes.getAs[Boolean]("input").getOrElse(false)
      val output = item.attributes.getAs[Boolean]("output").getOrElse(false)
      val op = item.isInstanceOf[Operation]
      if(op) "operation"
      else if(input) "input"
      else if(output) "output"
      else "internal"
    }

    def toggleForce(runnerID: ID, id: ID) = {
      $.modState{s =>
        val isForce = s.activeForce.contains(id)

        val newState =
          if(isForce) s.copy(activeForce = s.activeForce - id)
          else s.copy(activeForce = s.activeForce + id)

        // send new force table to backend
        val f = newState.force.filterKeys(newState.activeForce.contains)
        send(api.SetForceTable(runnerID, f, s.forceEvents))

        newState
      }
    }

    def renderState(p: Props, s: State) = {
      <.table(
        ^.className := "table table-striped",
        ^.width:="900px",
        <.thead(
          <.tr(
            <.th(^.width:="200px","Name"),
            <.th(^.width:="200px","Value"),
            <.th(^.width:="30px","Type"),
            <.th(^.width:="200px","Forced value"),
            <.th(^.width:="200px","Force"),
            <.th(^.width:="200px","Active events"),
          )
        ),
        <.tbody( {
          val rows = for {
            runnerID <- p.activeRunner.toList
            state <- p.runnerStates.get(runnerID)
            model <- p.activeModel
          } yield {
            val items = for {
              (k,v) <- state
              item <- model.items.get(k)
              if item.name.toLowerCase.contains(s.idableFilter.toLowerCase)
              if s.typeFilter == typeFilters.head || s.typeFilter == itemType(item)
            } yield {
              (item,v)
            }
            items.map { case (item, v) =>
              val domain = item.attributes.getAs[List[SPValue]]("domain").getOrElse(List())
              val dd = domain.map(d => <.div(d.toString, ^.onClick --> setForce(runnerID, item.id, d)))
              val selectedForce = s.force.get(item.id).getOrElse(SPValue("[set force]"))
              val activeForce = s.activeForce.contains(item.id)

              val events = List("[none]", "start", "reset", "forceReset")
              val ed = events.map(e => <.div(e, ^.onClick --> setEvent(runnerID, item.id, e)))
              val selectedEvent = s.forceEvents.get(item.id).getOrElse(SPValue("[none]"))

              val forceButtonStyle =
                if (activeForce) css.activeModelButton.htmlClass
                else css.inactiveModelButton.htmlClass

              <.tr(
                <.td(item.name),
                <.td(v.toString),
                <.td(itemType(item)),
                <.td(SPWidgetElements.dropdown(selectedForce.toString, dd)),
                <.td(
                  <.button(
                    ^.className := s"btn btn-sm ${forceButtonStyle}",
                    ^.title := "Force value",
                    ^.onClick --> toggleForce(runnerID, item.id),
                    <.i(^.className := (if (activeForce) "fa fa-circle" else "fa fa-circle-thin")),
                  )
                ),
                <.td(
                  SPWidgetElements.dropdown(selectedEvent.toString, ed).when(item.isInstanceOf[Operation])
                ),
              )
            }.toTagMod
          }
          rows.toTagMod
        })
      )
    }

    def onFilterChange(e: ReactEventFromInput) = {
      val newValue = e.target.value
      $.modState(_.copy(idableFilter = newValue))
    }

    def setTypeFilter(t: String) = {
      $.modState(_.copy(typeFilter = t))
    }

    def render(p: Props, s: State) = {
      val typeLinks = typeFilters.map(t => <.div(t, ^.onClick --> setTypeFilter(t)))

      <.div(
        p.activeRunner.map { runnerID =>
          <.div(
          <.button(
            ^.className := "btn btn-small",
            ^.onClick --> send(api.StartAuto(runnerID)), "start auto"
          ),
          <.button(
            ^.className := "btn btn-small",
            ^.onClick --> send(api.StopAuto(runnerID)), "stop auto"
          ))
        }.toList.toTagMod,
        <.label("Filter: "),
        <.input(
          ^.width := "150px",
          ^.value := s.idableFilter,
          ^.onChange ==> onFilterChange
        ),
        SPWidgetElements.dropdown(s.typeFilter, typeLinks),
        p.activeRunner.map(id => "Active runner: " + id.toString).toList.toTagMod,
        renderState(p, s)
      )
    }

    def onUnmount() = {
      println("Unmounting")
      //messObs.kill()
      Callback.empty
    }
  }

  def send(mess: api.Request): Callback = {
    RunnerManagerCommunication.postRequest(mess)
    Callback.empty
  }

  val connectCircuit: ReactConnectProxy[FrontendState] = MainCircuit.connectComponent(identity)

  private val component = ScalaComponent.builder[Props]("RunnerStateWidgetState")
    .initialState(State(Map(), Set(), Map()))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(_ => connectCircuit { proxy =>  component(Props(proxy)) })

}
