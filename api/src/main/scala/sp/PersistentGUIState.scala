package sp

import sp.domain.{JSFormat, SPValue}

/**
  * Holds data types related to persisting GUI state.
  */
object PersistentGUIState {
  case class GUICommand(mapping: State => State, saveSnapshot: Boolean = false)

  trait WidgetId

  trait Key
  case class UniqueKey(id: String)
  case class WidgetKey(widgetId: WidgetId)

  type Store = Map[Key, SPValue]

  case class State(store: Map[Key, SPValue] = Map()) {
    def update(f: State => State): State = f(this)
  }

  val AkkaTopic = "persist-gui-state"

  object Formats {
    import play.api.libs.json._
  }

  implicit lazy val fStore: JSFormat[Store] =
}
