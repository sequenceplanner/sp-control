package spgui.widgets.OPGUI

import sp.domain._
import sp.domain.Logic._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import sp.abilityhandler.APIAbilityHandler
import sp.abilityhandler.APIAbilityHandler.GetAbility
import sp.runners.APIOperationRunner
import sp.runners.APIOperationRunner.Setup
import sp.vdtesting.APIVDTracker
import spgui.communication._
import sp.models.APIModel
import spgui.widgets.OPGUI
import spgui.widgets.OPGUI.OperationRunnerCardComponent.RunnerCard

/** Widget to visualize the pairs of abilities/operations */
object OperationRunnerWidget {
  // Case class for a ability and its state
  case class AbilityWithState(ability: APIAbilityHandler.Ability, abilityState: Map[ID, SPValue])
  // Case class for a operation and its state
  case class OperationWithState(operation: Operation, operationState: Map[ID, SPValue])
  // The runner
  case class Runner(id: Option[ID] = None, runInAuto: Boolean = true,
                    startOperation: Option[ID] = None, stepBackward: Boolean = false)

  /** We need to separate the activeCards (the pairs the runner is using)
    * and the Operation/Ability-pair available, which we later can add to the runner
    *
    * @param activeRunner The runner
    * @param modelIdables All IDables from the model
    * @param abilityStateMapper All ability-information needed
    * @param operationStateMapper All operation-information needed
    * @param activeOpAbPairs The map between operation id and ability id, that the runner has
    * @param operationAbilityMap
    * @param showAvailablePairs
    * @param availableOpAbPairs
    * @param otherSetups
    */
  case class State(
                    activeRunner:           Option[Runner] = None,
                    modelIdables:           List[IDAble] = List(),
                    abilityStateMapper:     Map[ID, AbilityWithState] = Map(),
                    operationStateMapper:   Map[ID, OperationWithState] = Map(),
                    activeOpAbPairs:        Map[ID, ID] = Map(), // in the runner
                    operationAbilityMap:    Map[ID, ID] = Map(),
                    showAvailablePairs:     Boolean = false,
                    availableOpAbPairs:     Map[ID, ID] = Map(),
                    otherSetups:            List[Setup] = List()
                  )

  private class Backend($: BackendScope[Unit, State]) {
    val operationRunnerHandler =
      BackendCommunication.getMessageObserver(onOperationRunnerMessage, APIOperationRunner.topicResponse)
    val abilityHandler =
      BackendCommunication.getMessageObserver(onAbilityMessage, APIAbilityHandler.topicResponse)
    // TODO: Listen for "global" runner instead of latest?
    val vdTrackerHandler =
      BackendCommunication.getMessageObserver(onVDTrackerMessage, APIVDTracker.topicResponse)
    val modelHandler =
      BackendCommunication.getMessageObserver(onModelMessage, APIModel.topicResponse)

    // TODO: Listen for "global" runner instead of latest?
    /** Handle APIVDTracker-messages.
      *
      * When a runner is launched in VDTracker with [[APIVDTracker.OpRunnerCreated]],
      * trigger a[[APIOperationRunner.GetRunners]] request.
      *
      * Update the state with the activeRunnerID from the runner that is created
      *
      * @param mess SPMessage
      */
    def onVDTrackerMessage(mess: SPMessage): Unit = {
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

    /** Handle APIOperationRunner-messages.
      *
      * If something else, Empty Callback.
      *
      * @param mess SPMessage from APIOperationRunner
      */
    def onOperationRunnerMessage(mess: SPMessage): Unit = {
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

            // try to update state with operationStateMapper and opAbPairs
            sameRunnerSetup.map{setup =>
              val (operationStates, opAbPairs) = parse(setup)
              // Calls to backend
              opAbPairs.values.foreach{abilityID => sendToAbilityHandler(APIAbilityHandler.GetAbility(abilityID))}

              state.copy(
                activeOpAbPairs = state.activeOpAbPairs ++ opAbPairs,
                operationStateMapper = state.operationStateMapper ++ operationStates,
                operationAbilityMap = state.operationAbilityMap ++ setup.opAbilityMap,
                otherSetups = state.otherSetups ++ otherSetups)
            }.getOrElse(state)
          }
        }

        // TODO: Add runInAuto and disableConditionGroups
        // case StateEvent-message: see if any operation has been updated
        // if so, update the operationStateMapper
        case APIOperationRunner.StateEvent(runnerID, newRunnerStateMap, _, _) => {
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

    /**
      * map each operationId in [[Setup.ops]]: Set[Operation] to
      * a new OperationWithState with a empty state: Map[ID, SPValue
      * @param setup: Setup - the [[APIOperationRunner.Setup]] that the runner have
      * @return the map of operationID -> OperationWithState
      */
    def opStateMapper(setup: Setup): Map[ID, OperationWithState] =
      setup.ops.map(operation => operation.id -> OperationWithState(operation, Map())).toMap

    /** Parse the setup
      *
      * @param setup A runner
      * @return Pair of all operations from the setup, and map between
      */
    def parse(setup: Setup): (Map[ID, OperationRunnerWidget.OperationWithState], Map[ID, ID]) =
      (opStateMapper(setup), setup.opAbilityMap)


    /** Handle APIAbilityHandler-messages.
      *
      * If a [[APIAbilityHandler.AbilityState]] response is noticed,
      * update the abilityState in the AbilityWithState-map.
      *
      * If a [[APIAbilityHandler.TheAbility]] response is noticed,
      * add the ability to the abilityStateMapper.
      *
      * If something else, Empty Callback.
      *
      * @param mess SPMessage from APIAbilityHandler
      */
    def onAbilityMessage(mess: SPMessage): Unit = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIAbilityHandler.Response].map {
        case APIAbilityHandler.AbilityState(id, newAbilityState) => {
          $.modState { state =>
            state.copy(abilityStateMapper = state.abilityStateMapper.map{ abs =>
              if(abs._1 == id) abs._1 -> abs._2.copy(abilityState = newAbilityState) else abs
            })
          }
        }
        case APIAbilityHandler.TheAbility(ability) =>
          $.modState { state =>
            ability.map{ ab =>
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
      callback.foreach(_.runNow())
    }

    /** Handle APIModel-messages.
      *
      * If a [[APIModel.SPItems]] response is noticed,
      * update the local lists of all IDables.
      *
      * If something else, Empty Callback.
      *
      * @param mess SPMessage from APIModel
      */
    def onModelMessage(mess: SPMessage): Unit = {
      val callback: Option[CallbackTo[Unit]] = mess.getBodyAs[APIModel.Response].map{
        case APIModel.SPItems(items) => {
          $.modState(_.copy(modelIdables = items))
        }
        case x => Callback.empty
      }
      callback.foreach(_.runNow())
    }

    /** Set SPHeader and send SPMessage to APIOperationRunner
      *
      * @param mess APIOperationRunner-Request
      */
    def sendToRunner(mess: APIOperationRunner.Request): Unit = {
      val header = SPHeader(from = "OperationRunnerWidget", to = "", reply = SPValue("OperationRunnerWidget"))
      BackendCommunication.publish(SPMessage.make(header, mess), APIOperationRunner.topicRequest)
    }

    /** Set SPHeader and send SPMessage to APIAbilityHandler
      *
      * @param mess APIAbilityHandler-Request
      */
    def sendToAbilityHandler(mess: APIAbilityHandler.Request): Unit = {
      val header = SPHeader(from = "OperationRunnerWidget", to = APIAbilityHandler.service,
        reply = SPValue("OperationRunnerWidget"), reqID = java.util.UUID.randomUUID())
      BackendCommunication.publish(SPMessage.make(header, mess), APIAbilityHandler.topicRequest)
    }

    /** Render-function in Backend.
      *
      * Make a OperationRunnerCardComponent and for all the active opAbPairs, map it against a DriverCard.
      *
      * @param state Current state in Backend-class
      * @return The Widget GUI
      */
    def render(state: State) = {
      <.div(
        <.button(
          ^.onClick --> Callback{showAvailablePairsAction(state)},
          "Show Available Pairs"
        ),
        <.div(
          ^.className := OperationRunnerWidgetCSS.widgetRoot.htmlClass,
          OperationRunnerCardComponent(
            state.modelIdables, {
              val opAbCards: List[RunnerCard] = state.activeOpAbPairs.map { operationAbilityPair => {
                val op = state.operationStateMapper(operationAbilityPair._2)
                val ab = state.abilityStateMapper(operationAbilityPair._1)
                OperationRunnerCardComponent.OperationRunnerCard(op.operation.id, ab, op)
              }
              }.toList
              // TODO: Fix the backend issue with operations as things and no information about the state of the operation
              val operationThings = state.modelIdables.filter{_.attributes.keys.contains("domain")}
              val operations = operationThings.map{opThing =>
                opThing.attributes.getAs[Set[Operation]]("ops").getOrElse(Set())
              }.flatten
              val unusedOperations = operations.filterNot{op =>
                state.activeOpAbPairs.contains(op.id)
              }
              val lonelyOpCards = unusedOperations.map{op =>
                OperationRunnerCardComponent.OperationRunnerLonelyOp(
                  op.id,
                  OperationWithState(op, Map())
                )
              }
              //            val operations = state.modelIdables.collect {case o: Operation => o}
              /*println(s"Things: $things \n" +
                s"Operations: $operations")*/
              /* val lonelyThings = operationThings.filterNot{opThing => state.operationAbilityMap.contains(opThing.id)}
               val lonelyOperationMap: Map[ID, OperationWithState] =
                 state.operationStateMapper.filterNot{operationWithState => state.operationAbilityMap.contains(operationWithState._1)}

               */
              /* val lonelyOpCards: List[RunnerCard] = lonelyThings.map{thing =>
                 val newOperation = Operation(name = thing.name,conditions = List(), attributes = thing.attributes, id = thing.id )
                 OperationRunnerCardComponent.OperationRunnerLonelyOp(
                   thing.id,
                   OperationWithState(newOperation, Map())
                 )

               }*/

              val lonelyAbilityMap = state.abilityStateMapper.filterNot{ability =>
                state.activeOpAbPairs.contains(ability._1)
              }

              val lonelyAbCards: List[RunnerCard] = lonelyAbilityMap.map{ab =>
                OperationRunnerCardComponent.OperationRunnerLonelyAb(
                  ab._1, ab._2
                )
              }.toList

              val availableCards: List[RunnerCard] = state.availableOpAbPairs.map { operationAbilityPair => {
                val op = state.operationStateMapper(operationAbilityPair._2)
                val ab = state.abilityStateMapper(operationAbilityPair._1)
                OperationRunnerCardComponent.AvailableCard(op.operation.id, ab, op)
              }
              }.toList
              val mergeCards: List[RunnerCard] = opAbCards ++ lonelyOpCards ++ lonelyAbCards
              if(state.showAvailablePairs && availableCards.nonEmpty) mergeCards ++ availableCards else mergeCards
            }
          )
        )
      )
    }

    def showAvailablePairsAction(state: State): Unit = {
      val (listOfAllOperations, listOfAllPairs) = state.otherSetups.map(parse).unzip
      // Reduce the lists of Map to one list with merging ++
      val allOtherPairs: Map[ID, ID] = listOfAllPairs.reduce(_ ++ _)
      // Call Get Ability
      allOtherPairs.values.foreach{abilityID => sendToAbilityHandler(APIAbilityHandler.GetAbility(abilityID))}
      state.copy(
        showAvailablePairs = !state.showAvailablePairs,
        operationStateMapper = state.operationStateMapper ++ listOfAllOperations.reduce(_ ++ _),
        availableOpAbPairs = state.availableOpAbPairs ++ allOtherPairs
      )
    }

    /** When the widget is unmounting, kill message-observer
      *
      * @return Callback to kill message-Observers
      */
    def onUnmount: Callback = Callback{
      println("OperationRunnerWidget Unmouting")
      operationRunnerHandler.kill()
      abilityHandler.kill()
      vdTrackerHandler.kill()
      modelHandler.kill()
    }
  }

  private val operationRunnerComponent = ScalaComponent.builder[Unit]("OperationRunnerWidget")
    .initialState(State())
    .renderBackend[Backend]
    .componentWillUnmount(_.backend.onUnmount)
    .build

  def apply() = spgui.SPWidget(spwb => operationRunnerComponent())
}
