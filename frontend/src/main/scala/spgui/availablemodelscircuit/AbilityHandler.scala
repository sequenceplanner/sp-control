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


class AbilityHandler[M](modelRW: ModelRW[M, AbilityHandlerState]) extends ActionHandler(modelRW) {
  import AbilityHandlerState.abilities

  type StateFn = AbilityHandlerState => AbilityHandlerState

  private def handleAction: PartialFunction[AbilityAction, StateFn] = {
    case AddAbility(ability) => abilities.modify(_ :+ ability)
    case AddAbilities(newAbilities) => abilities.modify(_ ++ newAbilities)
  }

  override protected def handle: PartialFunction[Any, ActionResult[M]] = {
    case action: AbilityAction => updated(handleAction(action)(value))
  }
}