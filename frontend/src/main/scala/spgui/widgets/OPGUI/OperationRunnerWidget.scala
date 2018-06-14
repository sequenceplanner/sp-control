package spgui.widgets.OPGUI

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.abilityhandler.APIAbilityHandler
import sp.domain._
import sp.runners.APIOperationRunner
import sp.runners.APIOperationRunner.Setup
import spgui.communication._

object OperationRunnerWidget {

  case class AbilityWithState(ability: APIAbilityHandler.Ability, abilityState: Map[ID, SPValue])
  case class OperationWithState(operation: Operation, operationState: Map[ID, SPValue])
  // In OperationRunnerWidget, we want to visualize the pairs of abilities/operations
  case class OpAbPair(
                       abilityID:       ID,
                       operationID:     ID
                     )

  // we need to separate the activeCards (the pairs the runner is using)
  // and the Operation/Ability-pair available, which we later can activate to the runner
  case class State(
                    activeRunnerID:       ID,
                    abilityStateMapper:   Map[ID, AbilityWithState],
                    operationStateMapper: Map[ID, OperationWithState],
                    activeOpAbPairs:      List[OpAbPair], // in the runner
                    availableOpAbPairs:   List[OpAbPair]  // in the model with possibility to add to runner
                  )

  private class Backend($: BackendScope[Unit, State]) {
    val operationRunnerHandler =
      BackendCommunication.getMessageObserver(onOperationRunnerMessage, APIOperationRunner.topicResponse)
    val abilityHandler =
      BackendCommunication.getMessageObserver(onAbilityMessage, APIAbilityHandler.topicResponse)

    def onOperationRunnerMessage(mess: SPMessage) = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIOperationRunner.Response].map {
        // case Runners-message: create new opAbPairs
        case APIOperationRunner.Runners(ids) => {
          $.modState { state =>
            // with a setup, go through the ops: Set[Operation] and map the operationId with a abilityID
            def parseSetup(setup: Setup): Set[OpAbPair] = setup.ops.flatMap { operation =>
              setup.opAbilityMap
                .get(operation.id)
                .map { abilityID =>
                  sendToAbilityHandler(APIAbilityHandler.GetAbility(abilityID))
                  OpAbPair(abilityID, operation.id)
                }
            }
            // find the setup with the same runner-id as the widgets activeRunnerId
            //
            ids.find { setup => setup.runnerID == state.activeRunnerID}
              .map { setup =>
                state.copy(activeOpAbPairs = parseSetup(setup).toList)
              }.getOrElse(state)
          }
        }
        case APIOperationRunner.StateEvent(runnerID, newRunnerStateMap) => {
          $.modState{ state =>
            // if StateEvent occur, check if the operationState is for the same Runner-id as the widgets activeRunnerId
            if (state.activeRunnerID == runnerID) {
              // filter newRunnerStateMap on the operationStateMapperKeys
              // This way we sure we only update the new operation states
              val existingOperations = newRunnerStateMap.filterKeys(key => state.operationStateMapper.keySet.contains(key))
              // for each object in operationStateMapper
              // update the operationState for the operation,
              // if it has been changed with newRunnerStateMap
              // else keep old value
              val updatedOperationStateMapper = state.operationStateMapper.map(oneOperationWithState =>
                oneOperationWithState._1 -> oneOperationWithState._2.copy(
                  operationState = if(existingOperations.keySet.contains(oneOperationWithState._1))
                    Map(oneOperationWithState._1 -> existingOperations(oneOperationWithState._1))
                  else
                    oneOperationWithState._2.operationState)
              )
              // copy the state with the updated operationStateMapper
              state.copy(operationStateMapper = updatedOperationStateMapper)
            }
            else state // if the StateEvent is for another Runner, do not modify state
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
            if (ability.isDefined)
              // if ability is Defined add it (or overwrite old values) to the abilityStateMap
              // and map it with a new ability and wait for next AbilityState()
              state.copy(abilityStateMapper = state.abilityStateMapper + (ability.get.id -> AbilityWithState(ability.get, Map())))
            else
              // else if ability is not defined, GetAbility(id) has not been able to find
              // the ability with that id in the AbilityStorage list => do not change state
              state
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

      )
    }


    def onUnmount() = Callback{
      println("OperationRunnerWidget Unmouting")
      operationRunnerHandler.kill()
    }
  }

  private val operationRunnerComponent = ScalaComponent.builder[Unit]("OperationRunnerWidget")
    .initialState(State(ID.newID, Map(), Map(), List(), List()))
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount())
    .build

  def apply() = spgui.SPWidget(spwb => operationRunnerComponent())
}





