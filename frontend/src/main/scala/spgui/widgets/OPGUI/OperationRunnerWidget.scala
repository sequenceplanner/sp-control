package spgui.widgets.OPGUI

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.abilityhandler.APIAbilityHandler
import sp.runners.APIOperationRunner
import sp.runners.APIOperationRunner.Setup
import sp.vdtesting.APIVDTracker
import spgui.communication._
import sp.models.{APIModel => mapi}

// In OperationRunnerWidget, we want to visualize the pairs of abilities/operations
object OperationRunnerWidget {
  // Case class for a ability and its state
  case class AbilityWithState(ability: APIAbilityHandler.Ability, abilityState: Map[ID, SPValue])
  // Case class for a operation and its state
  case class OperationWithState(operation: Operation, operationState: Map[ID, SPValue])

  case class Runner(id: Option[ID] = None, runInAuto: Boolean = true,
                    startOperation: Option[ID] = None, stepBackward: Boolean = false)
  // Pairs of ID:s from the Runner.Setup.opAbilityMap
  case class OpAbPair(abilityID: ID, operationID: ID)

  // we need to separate the activeCards (the pairs the runner is using)
  // and the Operation/Ability-pair available, which we later can activate to the runner
  case class State(
    activeRunner:         Option[Runner] = None,
    modelIdables:         List[IDAble] = List(),
    abilityStateMapper:   Map[ID, AbilityWithState] = Map(),
    operationStateMapper: Map[ID, OperationWithState] = Map(),
    activeOpAbPairs:      List[OpAbPair] = List(), // in the runner
    availableOpAbPairs:   List[OpAbPair] = List() // in the model with possibility to add to runner
  )

  private class Backend($: BackendScope[Unit, State]) {
    val operationRunnerHandler =
      BackendCommunication.getMessageObserver(onOperationRunnerMessage, APIOperationRunner.topicResponse)
    val abilityHandler =
      BackendCommunication.getMessageObserver(onAbilityMessage, APIAbilityHandler.topicResponse)
    // TODO: Listen for "global" runner instead of latest?

    val vdTrackerHandler =
      BackendCommunication.getMessageObserver(onVDTrackerMessage, APIVDTracker.topicResponse)

    val modelMessObs =
      BackendCommunication.getMessageObserver(onModelObsMes, mapi.topicResponse)

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
          // Update the state with the activeRunnerID from the runner that is created
          $.modState { state => state.copy(activeRunner = Some(Runner(Some(id)))) }

        case x => Callback.empty
      }
      // for each callback, runNow()
      callback.foreach(_.runNow())
    }

    def onOperationRunnerMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIOperationRunner.Response].map {
        // case Runners-message: create new opAbPairs
        case APIOperationRunner.Runners(ids) => {
          $.modState { state =>

            // try to find the setup with the same runner-id as the widgets activeRunner.id
            val sameRunnerSetup: Option[Setup] = ids.find(setup =>
              state.activeRunner.exists(_.id.contains(setup.runnerID)
              )
            )

            // TODO: add available Operation-Ability Cards
            // the otherSetups filters out sameRunnerSetup
            val otherSetups: List[Setup] = ids.filterNot(id => sameRunnerSetup.contains(id))

            /**
              * With the setup go through the [[Setup.ops]]: Set[Operation]
              * foreach operation get the ability that is mapped
              * with this operation in [[Setup.opAbilityMap]]
              * @param setup: Setup - the [[APIOperationRunner.Setup]] that the runner have
              * @return Set of [[OperationRunnerWidget.OpAbPair]]
              */
            def opAbPairs(setup: Setup): Set[OpAbPair] = setup.ops.flatMap { operation =>
              setup.opAbilityMap.get(operation.id).map { abilityID =>
                // Send [[APIAbilityHandler.GetAbility(id)]] with the abilityId
                sendToAbilityHandler(APIAbilityHandler.GetAbility(abilityID))
                // Create a new Pair
                OpAbPair(abilityID, operation.id)
              }
            }

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
            sameRunnerSetup.map{setup => state.copy(
              activeOpAbPairs = opAbPairs(setup).toList,
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

    def onAbilityMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIAbilityHandler.Response].map {
        // case AbilityState-message: update the abilityState in the AbilityWithState-map
        case APIAbilityHandler.AbilityState(id, newAbilityState) => {
          // if AbilityState occur, check if the abilityState is for the same Runner-id as the widgets activeRunnerId
          $.modState { state =>
            /*  for each object in abilityStateMapper
             if the updated ability have the same id as the object
             true - map it against the newAbilityState
             false - do not update state
             */
            if (state.abilityStateMapper.contains(id)) {
              val updatedAbility = state.abilityStateMapper(id).copy(abilityState = newAbilityState)
              state.copy(abilityStateMapper = state.abilityStateMapper + (id -> updatedAbility))
            } else {
              state
            }
          }
        }

        case APIAbilityHandler.TheAbility(ability) =>
          $.modState { state =>
            ability.map{ ab =>
              // if map contains ability, update abilityStateMapper
              state.copy(abilityStateMapper = state.abilityStateMapper + (ab.id -> AbilityWithState(ab, Map())))
            }.getOrElse(state) // else, do not update state
          }

        // Not need right now, delete?
        // If we get a list of Abilities, Create a GetAbility-call for each abilityID
        case APIAbilityHandler.Abilities(abilities) =>
          $.modState {
            state => {
              // call [[APIAbilityHandler.GetAbility(id)]]
              abilities.foreach { ability => sendToAbilityHandler(APIAbilityHandler.GetAbility(ability.id)) }
              // do not update state
              state
            }
          }
        case x => Callback.empty
      }
      // for each callback, runNow()
      callback.foreach(_.runNow())
    }

    def onModelObsMes(mess: SPMessage): Unit = {
      mess.body.to[mapi.Response].map{
        case mapi.SPItems(items) => {
          $.modState(_.copy(modelIdables = items)).runNow()
        }
        case x =>
      }
    }

    // Set SPHeader and send SPMessage to APIOperationRunner
    def sendToRunner(mess: APIOperationRunner.Request): Unit = {
      val h = SPHeader(from = "OperationRunnerWidget", to = "", reply = SPValue("OperationRunnerWidget"))
      BackendCommunication.publish(SPMessage.make(h, mess), APIOperationRunner.topicRequest)
    }

    // Set SPHeader and send SPMessage to APIAbilityHandler
    def sendToAbilityHandler(mess: APIAbilityHandler.Request): Unit = {
      val h = SPHeader(from = "OperationRunnerWidget", to = APIAbilityHandler.service,
        reply = SPValue("OperationRunnerWidget"), reqID = java.util.UUID.randomUUID())
      BackendCommunication.publish(SPMessage.make(h, mess), APIAbilityHandler.topicRequest)
    }

    def render(state: State) = {
      <.div(
        ^.className := OperationRunnerWidgetCSS.widgetRoot.htmlClass,
        SPCardGrid(
          state.modelIdables, 
          state.activeOpAbPairs.map{ operationAbilityPair => {
            val op = state.operationStateMapper(operationAbilityPair.operationID)
            val ab = state.abilityStateMapper(operationAbilityPair.abilityID)
            SPCardGrid.OperationRunnerCard(op.operation.id, ab, op)
          }}
        )
      )
    }
     
    def onUnmount() = Callback{
      println("OperationRunnerWidget Unmouting")
      operationRunnerHandler.kill()
      abilityHandler.kill()
      vdTrackerHandler.kill()
    }
  }

  private val operationRunnerComponent = ScalaComponent.builder[Unit]("OperationRunnerWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => operationRunnerComponent())
}
