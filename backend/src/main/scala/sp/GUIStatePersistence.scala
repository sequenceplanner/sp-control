package sp

import akka.persistence.{PersistentActor, SnapshotOffer}
import sp.modelImport.oldDomain.SPValue

class GUIStatePersistence extends PersistentActor {
  override def receiveRecover: Receive = {
    case SnapshotOffer(metadata, snapshot) =>
      println("\n\n\n\n")
      println("SNAPSHOT HERE:")
      println(snapshot)
      println("\n\n\n\n")
  }

  trait WidgetId

  trait Key
  case class UniqueKey(id: String)
  case class WidgetKey(widgetId: WidgetId)

  type Store = Map[Key, SPValue]

  case class State(store: Map[Key, SPValue] = Map()) {
    def update(f: State => State): State = f(this)
  }

  override def receiveCommand: Receive = handleCommand(State())

  def handleCommand(state: State): Receive = {
    case GUICommand(f, shouldSaveSnapshot) =>
      val newState = state.update(f)
      if (shouldSaveSnapshot) saveSnapshot(newState)

      context become handleCommand(newState)
  }

  override def persistenceId: String = "gui-state-persistence-actor"

  case class GUICommand(mapping: State => State, saveSnapshot: Boolean = false)
}
