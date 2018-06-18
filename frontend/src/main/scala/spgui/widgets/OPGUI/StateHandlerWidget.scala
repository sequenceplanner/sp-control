package spgui.widgets.OPGUI

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom.html
import sp.devicehandler.VD
import sp.domain.Logic._
import sp.domain._
import sp.models.APIModel
import sp.runners.APIOperationRunner
import sp.runners.APIOperationRunner.Setup
import spgui.communication._

object StateHandlerWidget {
  // Case class for a operation and its state
  case class OperationWithState(operation: Operation, operationState: Map[ID, SPValue])
  case class Runner(id: Option[ID] = None, runInAuto: Boolean = true,
                    startOperation: Option[ID] = None, stepBackward: Boolean = false)
  case class DriverWithState(driver: VD.Driver, driverState: VD.DriverState)
  // Pairs of ID:s from the Runner.Setup.variableMap
  case class AbilityDriverPair(abilityID: ID, driverID: ID)

  case class State(
                    activeRunner:           Option[Runner] = None,
                    theModel:               List[IDAble] = List()
                  )

  private class Backend($: BackendScope[Unit, State]) {
    val modelMessObs =
      BackendCommunication.getMessageObserver(onModelObsMes, APIModel.topicResponse)

    def onModelObsMes(mess: SPMessage): Unit = {
      mess.body.to[APIModel.Response].map{
        case APIModel.SPItems(items) => {
          $.modState(_.copy(theModel = items)).runNow()
        }
        case x =>
      }
    }

    def extractVariablesFromModel(model: List[IDAble]): (List[Thing], List[Thing], Map[ID,ID]) = {
      // get runner
      val runnerSetupThings: List[Thing] = model.collect{case t: Thing if t.attributes.keys.contains("runnerID") => t}
      val runners: List[Setup] = runnerSetupThings.map(APIOperationRunner.runnerThingToSetup)

      val r = runners.headOption // assume one runner
      val mapping: Map[ID, ID] = r.map(_.variableMap).getOrElse(Map())
      //      val operationThings = model.collect{case t: Thing if t.attributes.keys.contains("init") => t}
      val driverThings = model.collect{case t: Thing if t.attributes.keys.contains("driverName") => t}
      val operationThings = model.collect{case t: Thing => t}
        .filterNot(t => driverThings.exists(_.id==t.id))

      (operationThings, driverThings, mapping)
    }

    def sendToModel(model: ID, mess: APIModel.Request) = Callback{ //  Send message to model
      val header = SPHeader(from = "StateHandlerWidget", to = model.toString,
        reply = SPValue("StateHandlerWidget"))
      BackendCommunication.publish(SPMessage.make(header, mess), APIModel.topicRequest)
    }

    def render(state: State): TagOf[html.Div] = {
      val extracted: (List[Thing], List[Thing], Map[ID, ID]) = extractVariablesFromModel(state.theModel)
      <.div(
        renderModel(state.theModel, operationThings = extracted._1, driverThings = extracted._2, operationDriverMap = extracted._3)
      )
    }

    def renderModel(theModel: List[IDAble], operationThings: List[Thing], driverThings: List[Thing], operationDriverMap: Map[ID, ID]) = {
      <.div(
        <.div(
          <.details(^.open := "open", ^.className := "details-pairs",
            <.summary("Operation-Driver Pairs"),
            <.table(
              ^.className := "table table-striped", ^.className := "table-pairs",
              tableHead(),
              <.tbody(
                operationDriverMap.map { idPair =>
                  val operation: Thing = operationThings.find(_.id == idPair._1).getOrElse(Thing("debug-op"))
                  val driver: Thing = driverThings.find(_.id == idPair._2).getOrElse(Thing("debug-driver"))
                  if(driver.name != "debug-driver" && operation.name == "debug-op") {
                    println(s"Pair is s$idPair")
                    val a = theModel.find(_.id == idPair._1)
                    print("Model: ")
                    println(a.getOrElse("error-not-found"))
                  }
                  <.tr(
                    <.td(operation.name),
                    <.td(operation.id.toString),
                    <.td("TODO"),// TODO: Read or Write or No master?
                    <.td(driver.name),
                    <.td(driver.id.toString)
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
                    <.td(driver.id.toString)
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
    }
  }

  private val stateHandlerComponent = ScalaComponent.builder[Unit]("StateHandlerWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => stateHandlerComponent())
}