package sp

import akka.persistence.{PersistentActor, SnapshotOffer}
import sp.PersistentGUIState.State
import sp.domain.{SPHeader, SPMessage, SPValue}

class GUIStatePersistence extends PersistentActor with sp.service.ServiceSupport {
  subscribe(PersistentGUIState.AkkaTopic)

  override def receiveRecover: Receive = {
    case SnapshotOffer(metadata, snapshot) =>
      println("\n\n\n\n")
      println("SNAPSHOT HERE:")
      println(snapshot)
      println("\n\n\n\n")
  }

  override def receiveCommand: Receive = handleCommand(State())

  def handleCommand(state: State): Receive = {
    case command: PersistentGUIState.GUICommand =>
      println("Received handleCommand(GUICommand)")

      val nextState = command match {
        case PersistentGUIState.SavePresetsCommand(presets) =>
          state.update { s =>
            val newStore = s.store + (PersistentGUIState.Keys.PersistPresets -> presets)
            s.copy(store = newStore)
          }
        case PersistentGUIState.LoadPresetsCommand =>
          val key = PersistentGUIState.Keys.PersistPresets
          state.get(key) match {
            case Some(res) => sendResponse(res)
            case None => println(s"There was nothing in the store at key $key")
          }

          state
      }

      if (command.saveSnapshot) saveSnapshot(nextState)
      context become handleCommand(nextState)

    case x => println(s"handleCommand() received|: $x")
  }


  def sendResponse(body: SPValue): Unit = {
    val header = SPHeader(from = "GUIStatePersistence", to = "DashboardPresetsComponent")
    val toSend = SPMessage.makeJson(header, body)
    val TopicName = "gui-snapshot"
    publish(TopicName, toSend)
  }

  // val messObs = BackendCommunication.getMessageObserver(processMessage, topic = BackendTopic.PatientDataEvent)

  override def persistenceId: String = "gui-state-persistence-actor"
}
