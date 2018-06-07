package sp

import sp.domain.SPValue
/**
  * Holds data types related to persisting GUI state.
  */
object PersistentGUIState {
  trait GUICommand {
    def saveSnapshot: Boolean
  }

  trait CommandSave extends GUICommand {
    override def saveSnapshot = true
  }

  trait Command extends GUICommand {
    override def saveSnapshot = false
  }

  case class SavePresetsCommand(presets: SPValue) extends CommandSave
  case object LoadPresetsCommand extends Command

  object Keys {
    val PersistPresets = "persist-presets"
  }

  type Store = Map[String, SPValue]

  case class State(store: Map[String, SPValue] = Map()) {
    def update(f: State => State): State = f(this)
    def get(key: String): Option[SPValue] = store.get(key)
  }

  val AkkaTopic = "persist-gui-state"

  object Formats {
    import play.api.libs.json._
  }

  // implicit lazy val fStore: JSFormat[Store] =
}
