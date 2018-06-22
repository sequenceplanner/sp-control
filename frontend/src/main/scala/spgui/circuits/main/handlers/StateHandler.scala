package spgui.circuits.main.handlers

import diode.{Action, ActionHandler, ActionResult, ModelRW}
import spgui.circuits.main.LocalAction

/**
  *
  * @tparam M Main state type
  * @tparam S Sub state type
  * @tparam A Action type
  */
abstract class StateHandler[M, S, A](modelRW: ModelRW[M, S]) extends ActionHandler(modelRW) {
  type StateFn = S => S

  /* This method is necessary to allow the subclasses to reject Actions of the wrong type.
   * The issue can be solved through reflection, but that unfortunately negatively impacts performance
   * and has therefore been avoided here.
   */
  def acceptAction: Action => Boolean

  override protected def handle: PartialFunction[Any, ActionResult[M]] = {
    case message: Action =>
      val (action, local) = message match {
        case LocalAction(newAction) => (newAction, true)
        case other => (other, false)
      }

      if (acceptAction(action)) runReaction(onAction(action.asInstanceOf[A]), local)
      else noChange
  }

  private def runReaction(reaction: Reaction, local: Boolean): ActionResult[M] = {
    reaction match {
      case GlobalReaction(_, runGlobally) if !local => runGlobally()
      case _ => Unit
    }

    updated(reaction.stateTransform(value))
  }

  def onAction: PartialFunction[A, Reaction]

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
