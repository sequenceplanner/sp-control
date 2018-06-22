package spgui.availablemodelscircuit
import diode.{Action, ActionHandler, ActionResult, ModelRW}

/**
  *
  * @tparam M Main state type
  * @tparam S Sub state type
  * @tparam A Action type
  */
abstract class StateHandler[M, S, A](modelRW: ModelRW[M, S]) extends ActionHandler(modelRW) {
  type StateFn = S => S

  override protected def handle: PartialFunction[Any, ActionResult[M]] = {
    case message: Action =>
      val (action, local) = message match {
        case LocalAction(newAction) => (newAction, true)
        case other => (other, false)
      }

      action match {
        case modelAction: A =>
          runReaction(getReaction(modelAction), local)
      }
  }

  private def runReaction(reaction: Reaction, local: Boolean): ActionResult[M] = {
    reaction match {
      case GlobalReaction(_, runGlobally) if !local => runGlobally()
      case _ => Unit
    }

    updated(reaction.stateTransform(value))
  }

  def getReaction: PartialFunction[A, Reaction]

  def react(stateTransformation: S => S) = LocalReaction(stateTransformation)

  case class LocalReaction(stateTransform: S => S) extends Reaction {
    def globally(f: => Unit) = GlobalReaction(stateTransform, () => f)
  }

  case class GlobalReaction(stateTransform: S => S, global: () => Unit) extends Reaction

  /**
    * Defines a DSL for dealing with handling diode actions in a way that separates effectless and effectful operations
    */
  trait Reaction {
    def stateTransform: S => S
  }

  object Reaction {
    case object None extends Reaction {
      val stateTransform: S => S = identity
    }
  }

  implicit def asStateFn(f: S => S): LocalReaction = LocalReaction(f)
}
