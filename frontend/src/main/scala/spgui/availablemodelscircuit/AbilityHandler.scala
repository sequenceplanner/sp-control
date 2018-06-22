package spgui.availablemodelscircuit

import diode.{Action, ActionHandler, ActionResult, ModelRW}
import monocle.macros.Lenses
import sp.abilityhandler.APIAbilityHandler.Ability


trait AbilityAction extends Action
case class AddAbility(ability: Ability) extends AbilityAction
case class AddAbilities(abilities: List[Ability]) extends AbilityAction


object AbilityHandler {
  val initialState: AbilityHandlerState = AbilityHandlerState(List())
}

@Lenses case class AbilityHandlerState(abilities: List[Ability])


class AbilityHandler[M](modelRW: ModelRW[M, AbilityHandlerState]) extends StateHandler[M, AbilityHandlerState, AbilityAction](modelRW) {
  import AbilityHandlerState.abilities

  override def getReaction: PartialFunction[AbilityAction, Reaction] = {
    case AddAbility(ability) => react {
      abilities.modify(_ :+ ability)
    }

    case AddAbilities(newAbilities) => react {
      abilities.modify(_ ++ newAbilities)
    }
  }
}