package sp

import akka.persistence.{PersistentActor, SnapshotOffer}
import sp.PersistentGUIState.State
import sp.domain.{SPHeader, SPMessage}

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
    case PersistentGUIState.GUICommand(f, shouldSaveSnapshot) =>
      val newState = state.update(f)
      if (shouldSaveSnapshot) saveSnapshot(newState)

      context become handleCommand(newState)
  }

  def sendEvent[A](header: SPHeader, body: A): Unit = {
    val toSend = SPMessage.makeJson(header, body)
    val TopicName = "" // TODO
    publish(TopicName, toSend)
  }

  // val messObs = BackendCommunication.getMessageObserver(processMessage, topic = BackendTopic.PatientDataEvent)

  override def persistenceId: String = "gui-state-persistence-actor"
}
