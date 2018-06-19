package spgui.widgets.OPGUI

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import sp.devicehandler.{APIDeviceDriver, VD}
import sp.domain.Logic._
import sp.domain._
import sp.models.APIModel
import sp.runners.APIOperationRunner
import sp.runners.APIOperationRunner.Setup
import spgui.communication._

object StateHandlerWidget {
  case class Runner(id: Option[ID] = None, runInAuto: Boolean = true,
                    startOperation: Option[ID] = None, stepBackward: Boolean = false)
  case class ExtractedThings(allOperations: List[Thing] = List(),
                             allDrivers: List[Thing] = List(), operation2Driver: Map[ID, ID] = Map())

  case class State(
                    activeRunner:           Option[Runner] = None,
                    theModel:               List[IDAble] = List(),
                    extractedThings:        ExtractedThings = ExtractedThings(),
                    driverStateMapper:      Map[ID, Map[String, SPValue]] = Map()
                  )

  private class Backend($: BackendScope[Unit, State]) {
    val modelMessObs =
      BackendCommunication.getMessageObserver(onModelMessage, APIModel.topicResponse)
    val deviceDriverHandler =
      BackendCommunication.getMessageObserver(onDriverMessage, APIDeviceDriver.topicResponse)

    /**
      * onModelMessage
      * @param mess SPMessage
      */
    def onModelMessage(mess: SPMessage): Unit = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIModel.Response].map {
        case APIModel.SPItems(items) => {
          $.modState { state =>
            // get all extracted things
            val e: ExtractedThings = extractVariablesFromModel(items)
            // for all drivers in extracted things
            // map it against a the driverState if it does already exist in driverStateMapper
            // else map it against a new string
            val driverStates: Map[ID, Map[String, SPValue]] = e.allDrivers.map{driver =>
              if (state.driverStateMapper.contains(driver.id))
                driver.id -> state.driverStateMapper(driver.id)
              else
                driver.id -> Map("Not connected yet..." -> SPValue(""))
            }.toMap
            // update state
            state.copy(theModel = items, extractedThings = e, driverStateMapper = state.driverStateMapper ++ driverStates)
          }
        }
        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    /**
      * filter out a list of operationThings, driverThings and the map between operations and drivers
      * @param model - list of [[IDAble]]
      * @return [[ExtractedThings]]
      */
    def extractVariablesFromModel(model: List[IDAble]): ExtractedThings = {
      // get runner
      val runnerSetupThings: List[Thing] = model.collect{case t: Thing if t.attributes.keys.contains("runnerID") => t}
      val runners: List[Setup] = runnerSetupThings.map(APIOperationRunner.runnerThingToSetup)

      val r = runners.headOption // assume one runner
      val mapping: Map[ID, ID] = r.map(_.variableMap).getOrElse(Map())
      val driverThings = model.collect{case t: Thing if t.attributes.keys.contains("driverName") => t}
      val operationThings = model.collect{case t: Thing if t.attributes.keys.contains("domain") => t}

      ExtractedThings(operationThings, driverThings, mapping)
    }

    /**
      * On Driver-message
      * @param mess SPMessage
      */
    def onDriverMessage(mess: SPMessage): Unit = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIDeviceDriver.Response].map {
        case APIDeviceDriver.DriverStateChange(_, id, newDriverState,_) => {
          $.modState { state =>
            // update only the drivers that the model has saved
            // so find existing driver in driverStateMapper and update the state
            val existingDriver: Option[(ID, Map[String, SPValue])] = state.driverStateMapper.find{_._1 == id}
            existingDriver.map { d: (ID, Map[String, SPValue]) =>
              state.copy(driverStateMapper = state.driverStateMapper + (d._1 -> newDriverState))
            }.getOrElse(state)
          }
        }
        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    /**
      * render-function in Backend
      * @param state
      * @return
      */
    def render(state: State): TagOf[html.Div] = {
      <.div(
        renderModel(state.theModel, state.extractedThings.allOperations,
          state.extractedThings.allDrivers, state.extractedThings.operation2Driver, state.driverStateMapper)
      )
    }

    /**
      * render the model in state handler
      * @param theModel
      * @param operationThings
      * @param driverThings
      * @param operationDriverMap
      * @param driverStates
      * @return
      */
    def renderModel(theModel: List[IDAble], operationThings: List[Thing], driverThings: List[Thing],
                    operationDriverMap: Map[ID, ID], driverStates: Map[ID, Map[String, SPValue]]) = {
      <.div(
        <.div(
          <.details(^.open := "open", ^.className := "details-pairs",
            <.summary("Operation-Driver Pairs"),
            <.table(
              ^.className := "table table-striped", ^.className := "table-pairs",
              tableHead(),
              <.tbody(
                operationDriverMap.map { idPair =>
                  val opVar: Thing = operationThings.find(_.id == idPair._1).getOrElse(Thing("debug-opVar"))
                  val driverVar: Thing = driverThings.find(_.id == idPair._2).getOrElse(Thing("debug-driverVar"))
                  <.tr(
                    <.td(opVar.name),
                    <.td(opVar.id.toString),
                    <.td("TODO"),// TODO: Read or Write or No master?
                    <.td(driverVar.name),
                    driverStates(driverVar.id).map(s => <.td(s._1 + ":" + s._2)).toTagMod
                  )
                }.toTagMod
              )
            )
          )
        ),
        <.div(
          <.details(^.open := "open",  ^.className := "details-empty-operations",
            <.summary("Operation with no Driver"),
            <.table(
              ^.className := "table table-striped",  ^.className := "table-empty-operations",
              tableHead(),
              <.tbody(
                operationThings.filterNot(thing => operationDriverMap.contains(thing.id)).map { operation =>
                  <.tr(
                    <.td(operation.name),
                    <.td(operation.id.toString),
                    <.td("TODO"),// TODO: Read or Write or No master?
                    <.td(""),
                    <.td("")
                  )
                }.toTagMod
              )
            )
          )
        ),
        <.div(
          <.details(^.open := "open", ^.className := "details-empty-drivers",
            <.summary("Driver with no Operation"),
            <.table(
              ^.className := "table table-striped", ^.className := "table-empty-drivers",
              tableHead(),
              <.tbody(
                driverThings.filterNot(thing => operationDriverMap.values.toList.contains(thing.id)).map { driver =>
                  <.tr(
                    <.td(),
                    <.td(),
                    <.td("TODO"),// TODO: Read or Write or No master?
                    <.td(driver.name),
                    driverStates(driver.id).map(s => <.td(s._1 + ":" + s._2)).toTagMod
                  )
                }.toTagMod
              )
            )
          )
        )
      )
    }

    def tableHead() = {
      <.thead(
        <.tr(
          <.td("Operation Name"),
          <.td("Operation ID"),
          <.td("Read/Write"),
          <.td("Driver Name"),
          <.td("Driver ID")
        )
      )
    }


    def onUnmount() = Callback{
      println("StateHandlerWidget Unmouting")
      modelMessObs.kill()
      deviceDriverHandler.kill()
    }
  }

  private val stateHandlerComponent = ScalaComponent.builder[Unit]("StateHandlerWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => stateHandlerComponent())
}
