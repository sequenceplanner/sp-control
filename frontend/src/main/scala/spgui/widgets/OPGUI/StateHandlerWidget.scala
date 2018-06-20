package spgui.widgets.OPGUI

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.devicehandler.{APIVirtualDevice}
import sp.domain.Logic._
import sp.domain._
import sp.models.APIModel
import sp.runners.APIOperationRunner
import sp.runners.APIOperationRunner.Setup
import spgui.communication._

/** Widget for matching the Driver with a Operation */
object StateHandlerWidget {
  case class Runner(id: Option[ID] = None, runInAuto: Boolean = true,
                    startOperation: Option[ID] = None, stepBackward: Boolean = false)
  case class ExtractedThings(allOperations: List[Thing] = List(),
                             allDrivers: List[Thing] = List(), operation2Driver: Map[ID, ID] = Map())

  /** The React-State of the Widget.
    * This Widget should be able to:
    *     1. stop the runner and go into manual-mode
    *     2. update the [[Thing]]:s from the model
    *     3. update the states of the VD
    *
    * @param activeRunner Current Runner
    * @param extractedThings The things from the model
    * @param virtualDeviceState The states of the Virtual Device
    */
  case class State(
                    activeRunner:           Option[Runner] = None,
                    extractedThings:        ExtractedThings = ExtractedThings(),
                    virtualDeviceState:     Map[ID, SPValue] = Map()
                  )

  private class Backend($: BackendScope[Unit, State]) {
    val modelHandler =
      BackendCommunication.getMessageObserver(onModelMessage, APIModel.topicResponse)
    val deviceDriverHandler =
      BackendCommunication.getMessageObserver(onVDMessage, APIVirtualDevice.topicResponse)

    /** Handle VirtualDevice-messages.
      *
      * If a [[APIModel.SPItems]] response is noticed,
      * update the local lists of driverThings and operationThings.
      *
      * If something else, Empty Callback.
      *
      * @param mess SPMessage from APIModel
      */
    def onModelMessage(mess: SPMessage): Unit = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIModel.Response].map {
        case APIModel.SPItems(items) => {
          $.modState { state =>
            // get all extracted things
            val e: ExtractedThings = extractVariablesFromModel(items)
            // for all drivers in extracted things
            // map it against a the driverState if it does already exist in driverStateMapper
            // else map it against a new ID
            val newDriverStates: Map[ID, SPValue] = e.allDrivers.map{driver =>
              if (state.virtualDeviceState.contains(driver.id))
                driver.id -> state.virtualDeviceState(driver.id)
              else
                driver.id -> SPValue("Not connected")
            }.toMap
            // update state
            state.copy(extractedThings = e, virtualDeviceState = state.virtualDeviceState ++ newDriverStates)
          }
        }
        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    /** Filter out a list of operationThings, driverThings and the map between operations and drivers
      *
      * @param model The model as a List of IDAble
      * @return The class ExtractedThings (containging the list of operationThings, driverThings and mapping)
      */
    def extractVariablesFromModel(model: List[IDAble]): ExtractedThings = {
      // get runner
      val runnerSetupThings: List[Thing] = model.collect{case t: Thing if t.attributes.keys.contains("runnerID") => t}
      val runners: List[Setup] = runnerSetupThings.map(APIOperationRunner.runnerThingToSetup)

      val r = runners.headOption // assume one runner
      val mapping: Map[ID, ID] = r.map(_.variableMap).getOrElse(Map())
      val driverThings = model.collect{case t: Thing if t.attributes.keys.contains("driverName") => t}
      val operationThings = model.collect{ case t: Thing if t.attributes.keys.contains("domain") => t}

      ExtractedThings(operationThings, driverThings, mapping)
    }

    /** Handle VirtualDevice-messages.
      *
      * If a [[APIVirtualDevice.StateEvent]] response is noticed,
      * update the local VD-states.
      *
      * If something else, Empty Callback.
      *
      * @param mess SPMessage from APIVirtualDevice
      */
    def onVDMessage(mess: SPMessage): Unit = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIVirtualDevice.Response].map {
        case APIVirtualDevice.StateEvent(_, id, newDriverStates,_) => {
          $.modState(state => state.copy(virtualDeviceState = state.virtualDeviceState ++ newDriverStates))
        }
        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    /** Render-function in Backend
      *
      * @param state Current state in Backend-class
      * @return The Widget GUI
      */
    def render(state: State) = {
      <.div(
        renderModel(state.extractedThings.allOperations,
          state.extractedThings.allDrivers, state.extractedThings.operation2Driver, state.virtualDeviceState)
      )
    }

    /** Render the model in state handler
      *
      * @param operationThings List of the operationThings in model
      * @param driverThings List of the driverThings in model
      * @param operationDriverMap The id:s of the operations that is connected to a driverValue. Map of [[ID]] to [[ID]].
      * @param virtualDeviceState The Driver-values. Map of [[ID]] to [[SPValue]])
      * @return The scene to the widget
      */
    def renderModel(operationThings: List[Thing], driverThings: List[Thing],
                    operationDriverMap: Map[ID, ID], virtualDeviceState: Map[ID, SPValue]) =
    {
      val sortedDriverlessOperationThings =
        operationThings.sortBy(t => t.name).filterNot(thing => operationDriverMap.contains(thing.id))
      val sortedOperationlessDriverThings =
        driverThings.sortBy(t => t.name).filterNot(thing => operationDriverMap.values.toList.contains(thing.id))

      <.div(
        <.div(
          <.details(^.open := "open", ^.className := "details-pairs",
            <.summary("Operation-Driver Pairs"),
            <.table(
              ^.className := "table table-striped", ^.className := "table-pairs",
              tableHead(),
              <.tbody(
                // for all pairs of operation-virtualDeviceState
                // print the things
                operationDriverMap.map { idPair =>
                  val opThing: Thing = operationThings.find(_.id == idPair._1).getOrElse(Thing("debug-opThing"))
                  val driverThing: Thing = driverThings.find(_.id == idPair._2).getOrElse(Thing("debug-driverThing"))
                  <.tr(
                    <.td(opThing.name),
                    <.td(opThing.id.toString),
                    <.td(""),// TODO: Read or Write or No master?
                    <.td(driverThing.name),
                    <.td(virtualDeviceState(driverThing.id).toString())
                  )
                }.toTagMod
              )
            )
          )
        ).when(operationDriverMap.nonEmpty),
        <.div(
          <.details(^.open := "open",  ^.className := "details-empty-operations",
            <.summary("Operation with no Driver"),
            <.table(
            ^.className := "table table-striped",  ^.className := "table-empty-operations",
            tableHead(),
              <.tbody(
                // for all operation things that do not have its id in operationDriverMap
                // print the operation
                sortedDriverlessOperationThings.map { operation =>
                  <.tr(
                    <.td(operation.name),
                    <.td(operation.id.toString),
                    <.td(""),// TODO: Read or Write or No master?
                    <.td(""),
                    <.td("")
                  )
                }.toTagMod
              )
            )
          )
        ).when(sortedDriverlessOperationThings.nonEmpty),
        <.div(
          <.details(^.open := "open", ^.className := "details-empty-drivers",
            <.summary("Driver with no Operation"),
            <.table(
              ^.className := "table table-striped", ^.className := "table-empty-drivers",
              tableHead(),
              <.tbody(
                // for all driver things that do not have its id in operationDriverMap
                // print the driver
                sortedOperationlessDriverThings.map { driverThing =>
                  <.tr(
                    <.td(),
                    <.td(),
                    <.td(""),// TODO: Read or Write or No master?
                    <.td(driverThing.name),
                    <.td(virtualDeviceState(driverThing.id).toString())
                  )
                }.toTagMod
              )
            )
          )
        ).when(sortedOperationlessDriverThings.nonEmpty)
      )
    }

    /** Table head for all tables used in widget
      *
      * @return A pre-defined <.thead(...)
      */
    def tableHead = {
      <.thead(
        <.tr(
          <.td("Operation Name"),
          <.td("Operation ID"),
          <.td("Read/Write"),
          <.td("Driver Name"),
          <.td("Driver Value")
        )
      )
    }

    /** When the widget is unmounting, kill message-observer
      *
      * @return Callback to kill message-Observers
      */
    def onUnmount = Callback{
      println("StateHandlerWidget Unmouting")
      modelHandler.kill()
      deviceDriverHandler.kill()
    }
  }

  private val stateHandlerComponent = ScalaComponent.builder[Unit]("StateHandlerWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount)
    .build

  def apply() = spgui.SPWidget(spwb => stateHandlerComponent())
}
