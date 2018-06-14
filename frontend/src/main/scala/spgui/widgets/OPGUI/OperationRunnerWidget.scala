package spgui.widgets.OPGUI

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.abilityhandler.APIAbilityHandler
import sp.domain._
import sp.runners.APIOperationRunner
import sp.runners.APIOperationRunner.Setup
import sp.vdtesting.APIVDTracker
import spgui.communication._
import spgui.widgets.VDGUI._

// In OperationRunnerWidget, we want to visualize the pairs of abilities/operations
object OperationRunnerWidget {
  // Case class for a ability and its state
  case class AbilityWithState(ability: APIAbilityHandler.Ability, abilityState: Map[ID, SPValue])
  // Case class for a operation and its state
  case class OperationWithState(operation: Operation, operationState: Map[ID, SPValue])
  // Pairs of ID:s from the Runner.Setup.opAbilityMap
  case class OpAbPair(abilityID: ID, operationID: ID)

  // we need to separate the activeCards (the pairs the runner is using)
  // and the Operation/Ability-pair available, which we later can activate to the runner
  case class State(
                    activeRunnerID:       Option[ID] = None,
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
    // TODO: Listen for "global" runner instead of latest
    val vdTrackerHandler =
      BackendCommunication.getMessageObserver(onVDTrackerMessage, APIVDTracker.topicResponse)

    def onVDTrackerMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIVDTracker.Response].map {
        case APIVDTracker.OpRunnerCreated(id) => {
          println(s"Publish GetRunners")
          sentToRunner(APIOperationRunner.GetRunners)
          println(s"opRunnerCreated by VDTracker with $id")
          $.modState { state => state.copy(activeRunnerID = Some(id)) }
        }
        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    def onOperationRunnerMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIOperationRunner.Response].map {
        // case Runners-message: create new opAbPairs
        case APIOperationRunner.Runners(ids) => {
          println(s"got API.Runners with $ids")
          $.modState { state =>
            // if current runner is defined update opAbPairs, else do nothing
            if (state.activeRunnerID.isDefined) {
              val isDef = state.activeRunnerID.isDefined; val runnerId = state.activeRunnerID
              println(s"runner is $isDef with value $runnerId ")
              // with a setup, go through the ops: Set[Operation] and map the operationId with a abilityID
              def parseSetup(setup: Setup): Set[OpAbPair] = setup.ops.flatMap { operation =>
                setup.opAbilityMap
                  .get(operation.id)
                  .map { abilityID =>
                    println(s"Publish GetAbility with abilityId: $abilityID")
                    sendToAbilityHandler(APIAbilityHandler.GetAbility(abilityID))
                    val a = OpAbPair(abilityID, operation.id)
                    println(s"OpAbPair Created $a")
                    a
                  }
              }
              // find the setup with the same runner-id as the widgets activeRunnerId
              //
              ids.find { setup => setup.runnerID == state.activeRunnerID.get }
                .map { setup =>
                  val opAbPairs = state.activeOpAbPairs
                  println(s"ActiveOpAbPairs is $opAbPairs")
                  state.copy(activeOpAbPairs = {
                    val set = parseSetup(setup)
                    val list = set.toList
                    println(s"parseSetup Returned $set")
                    println(s"parseSetup Returned $list")
                    parseSetup(setup).toList
                  })
                }.getOrElse(state)
            }
            else
              println("ActiveRunner is not defined in Runners")
            state
          }
        }

        case APIOperationRunner.StateEvent(runnerID, newRunnerStateMap) => {
          $.modState { state =>
            // if current runner is defined update operationStateMapper, else do nothing
            if (state.activeRunnerID.isDefined) {
              // if StateEvent occur, check if the operationState is for the same Runner-id as the widgets activeRunnerId
              if (state.activeRunnerID.get == runnerID) {
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
              }
              else state // if the StateEvent is for another Runner, do not modify state
            }
            else
              println("ActiveRunner is not defined in StateEvent")
            state
          }
        }

        case x => Callback.empty
      }
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
                false - map it against the old
              */
            if (state.abilityStateMapper.contains(id)) {
              val updatedAbility = state.abilityStateMapper(id).copy(abilityState = newAbilityState)
              state.copy(abilityStateMapper = state.abilityStateMapper + (id -> updatedAbility))
            } else
              state
          }
        }
        case APIAbilityHandler.TheAbility(ability) =>
          $.modState { state =>
            if (ability.isDefined) {
              println(s"ability is defined: $ability")
              // if ability is Defined add it (or overwrite old values) to the abilityStateMap
              // and map it with a new ability and wait for next AbilityState()
              state.copy(abilityStateMapper = state.abilityStateMapper + (ability.get.id -> AbilityWithState(ability.get, Map())))
            } else {
              // else if ability is not defined, GetAbility(id) has not been able to find
              // the ability with that id in the AbilityStorage list => do not change state
              println(s"Ability is not defined: $ability")
              state
            }
          }
        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    def sentToRunner(mess: APIOperationRunner.Request) = Callback{
      val h = SPHeader(from = "OperationRunnerWidget", to = "", reply = SPValue("OperationRunnerWidget"))
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, APIOperationRunner.topicRequest)
    }
    def sendToAbilityHandler(mess: APIAbilityHandler.Request) = Callback{
      val h = SPHeader(from = "OperationRunnerWidget", to = "OperationRunnerWidget", reply = SPValue("OperationRunnerWidget"))
      val json = SPMessage.make(h, mess)
      BackendCommunication.publish(json, APIAbilityHandler.topicRequest)
    }

    def render(state: State) = {
      <.div(
        SPCardGrid(
          state.activeOpAbPairs.map{ operationAbilityPair =>
            val a = state.abilityStateMapper(operationAbilityPair.abilityID)
            val o = state.operationStateMapper(operationAbilityPair.operationID)
            SPCardGrid.OperationRunnerCard(operationAbilityPair.operationID, a, o)
          }
        )
      )
    }


    def onUnmount() = Callback{
      println("OperationRunnerWidget Unmouting")
      operationRunnerHandler.kill()
    }
  }

  private val operationRunnerComponent = ScalaComponent.builder[Unit]("OperationRunnerWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => operationRunnerComponent())
}





