package spgui.widgets.OPGUI

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.TagOf
import org.scalajs.dom.html
import sp.abilityhandler.APIAbilityHandler
import sp.devicehandler.VD
import sp.domain._
import sp.domain.Logic._
import sp.models.APIModel
import sp.runners.APIOperationRunner
import sp.runners.APIOperationRunner.Setup
import sp.vdtesting.APIVDTracker
import spgui.communication._

object StateHandlerWidget {
  // Case class for a operation and its state
  case class OperationWithState(operation: Operation, operationState: Map[ID, SPValue])
  case class Runner(id: Option[ID] = None, runInAuto: Boolean = true,
                    startOperation: Option[ID] = None, stepBackward: Boolean = false)
  case class DriverWithState(driver: VD.Driver, driverState: VD.DriverState)
  // Pairs of ID:s from the Runner.Setup.variableMap
  case class AbilityVDModel(abilityID: ID, virtualDeviceID: ID)

  case class State(
                    activeRunner:           Option[Runner] = None,
                    operationStateMapper:   Map[ID, OperationWithState] = Map(),
                    driverStateMapper:      Map[ID, DriverWithState] = Map(),
                    theModel: List[IDAble] = List(),
                    abVdPairs:              List[AbilityVDModel] = List()       // in the runner
                  )

  private class Backend($: BackendScope[Unit, State]) {
    val operationHandler =
      BackendCommunication.getMessageObserver(onOperationRunnerWidget, APIOperationRunner.topicResponse)
    val vdTrackerHandler =
      BackendCommunication.getMessageObserver(onVDTrackerMessage, APIVDTracker.topicResponse)

    // TODO: Listen for "global" runner instead of latest?
    /**
      * When a runner is launched in VDTracker with [[APIVDTracker.OpRunnerCreated]]
      * trigger [[APIOperationRunner.GetRunners]] request
      * Update the state with the activeRunnerID from the runner that is created
      * @param mess sp.domain.SPMessage
      */
    def onVDTrackerMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIVDTracker.Response].map {

        case APIVDTracker.OpRunnerCreated(id) =>
          // trigger [[APIOperationRunner.GetRunners]] request
          sendToRunner(APIOperationRunner.GetRunners)
          // new runner with id
          val newRunner = Runner(Some(id))
          // Update the state with the activeRunnerID from the runner that is created
          $.modState{_.copy(activeRunner = Some(newRunner))}

        case x => Callback.empty
      }
      // for each callback, runNow()
      callback.foreach(_.runNow())
    }


    // TEMP to get our model
    import sp.models.{APIModel => mapi}
    val modelMessObs =
      BackendCommunication.getMessageObserver(onModelObsMes, mapi.topicResponse)
    def onModelObsMes(mess: SPMessage): Unit = {
      mess.body.to[mapi.Response].map{
        case mapi.SPItems(items) => {
          $.modState(_.copy(theModel = items)).runNow()
        }
        case x =>
      }
    }
    def getVars(m: List[IDAble]): (List[Thing], List[Thing], Map[ID,ID]) = {
      // get runner
      val runnerSetupThings = m.collect{case t: Thing if t.attributes.keys.contains("runnerID") => t}
      val runners = runnerSetupThings.map(APIOperationRunner.runnerThingToSetup)

      val r = runners.head // assume one runner
      val mapping = r.variableMap
      val opthings = m.collect{case t: Thing if t.attributes.keys.contains("init") => t}
      val dvthings = m.collect{case t: Thing if t.attributes.keys.contains("driverName") => t}


      (opthings, dvthings, mapping)
    }

    def onOperationRunnerWidget(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIOperationRunner.Response].map {
        // case Runners-message: create new opAbPairs
        case APIOperationRunner.Runners(ids) => {
          $.modState { state =>

            // try to find the setup with the same runner-id as the widgets activeRunner.id
            val setup: Option[Setup] = ids.find(setup =>
              state.activeRunner.exists(_.id.contains(setup.runnerID)
              )
            )

            /**
              * map each operationId in [[Setup.ops]]: Set[Operation] to
              * a new OperationWithState with a empty state: Map[ID, SPValue
              * @param setup: Setup - the [[APIOperationRunner.Setup]] that the runner have
              * @return the map of operationID -> OperationWithState
              */
            def opStateMapper(setup: Setup): Map[ID, OperationWithState] = {
              setup.ops.map(operation => operation.id -> OperationWithState(operation, Map())).toMap
            }

            // try to update state with operationStateMapper and opAbPairs
            setup.map{setup => state.copy(
              operationStateMapper = opStateMapper(setup))
            }.getOrElse(state)
          }
        }

        // TODO: Add runInAuto and disableConditionGroups
        // case StateEvent-message: see if any operation has been updated
        // if so, update the operationStateMapper
        case APIOperationRunner.StateEvent(runnerID, newRunnerStateMap, runInAuto, disableConditionGroups) => {
          $.modState { state =>
            // if StateEvent occur, check if the operationState is for the same Runner-id as the widgets activeRunnerId
            if(state.activeRunner.exists(_.id.contains(runnerID))){
              // filter newRunnerStateMap on the operationStateMapperKeys
              // This way we sure we only update the new operation states
              val existingOperations = newRunnerStateMap.filterKeys(key => state.operationStateMapper.keySet.contains(key))
              // for each object in operationStateMapper
              // update the operationState for the operation,
              // if it has been changed with newRunnerStateMap
              // else keep old value
              val updatedOperationStateMapper = state.operationStateMapper.map(oneOperationWithState =>
                oneOperationWithState._1 -> oneOperationWithState._2.copy(
                  operationState = if (existingOperations.keySet.contains(oneOperationWithState._1))
                    Map(oneOperationWithState._1 -> existingOperations(oneOperationWithState._1))
                  else
                    oneOperationWithState._2.operationState)
              )
              // copy the state with the updated operationStateMapper
              state.copy(operationStateMapper = updatedOperationStateMapper)
            } else {
              state // if the StateEvent is for another Runner, do not modify state}
            }
          }
        }

        // for the other messages i VDTracker, return Callback.empty
        case x => Callback.empty
      }
      // for each callback, runNow()
      callback.foreach(_.runNow())
    }

    // Set SPHeader and send SPMessage to APIOperationRunner
    def sendToRunner(mess: APIOperationRunner.Request): Unit = {
      val header = SPHeader(from = "OperationRunnerWidget", to = "", reply = SPValue("OperationRunnerWidget"))
      BackendCommunication.publish(SPMessage.make(header, mess), APIOperationRunner.topicRequest)
    }

    def sendToModel(model: ID, mess: APIModel.Request) = Callback{ //  Send message to model
      val header = SPHeader(from = "StateHandlerWidget", to = model.toString,
        reply = SPValue("StateHandlerWidget"))
      BackendCommunication.publish(SPMessage.make(header, mess), APIModel.topicRequest)
    }



    def render(state: State): TagOf[html.Div] = {
      <.div(
        <.table(
          <.thead(

          ),
          <.tbody(
            <.button(
              ^.onClick --> Callback{state.activeRunner.foreach(_.id.map(sendToModel(_,APIModel.GetItemList(0,99999))))},
              "Get Model IDAbles"
            ),
            <.div(
            state.operationStateMapper.map { op =>
              <.tr(
                <.td(op._2.operation.name),
                <.td("" + op._2.operation.id.toString)
              )
            }.toTagMod
            )
          )
        )


      )
    }

    def onUnmount() = Callback{
      println("StateHandlerWidget Unmouting")
      operationHandler.kill()
    }
  }

  private val stateHandlerComponent = ScalaComponent.builder[Unit]("StateHandlerWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => stateHandlerComponent())
}