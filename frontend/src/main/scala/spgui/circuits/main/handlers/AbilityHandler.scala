package spgui.circuits.main.handlers

import diode.{Action, ModelRW}
import monocle.macros.Lenses
import sp.AbilityStatus
import sp.abilityhandler.APIAbilityHandler.Ability
import sp.domain.{Condition, ID, SPAttributes, SPValue}
import spgui.SimpleSet


trait AbilityAction extends Action
case class AddAbility(ability: Ability) extends AbilityAction
case class AddAbilities(abilities: List[Ability]) extends AbilityAction
case class UpdateAbility(id: ID, f: AbilityData => AbilityData) extends AbilityAction

@Lenses case class AbilityHandlerState(abilities: SimpleSet[ID, AbilityData])

class AbilityHandler[M](modelRW: ModelRW[M, AbilityHandlerState]) extends StateHandler[M, AbilityHandlerState, AbilityAction](modelRW) {
  import AbilityHandlerState.abilities
  import SimpleSet.upsert

  override def onAction: PartialFunction[AbilityAction, Reaction] = {
    case AddAbility(ability) => react {
      abilities.modify(_ + AbilityData(ability))
    }

    case AddAbilities(newAbilities) => react {
      abilities.modify(_.addAll(newAbilities.map(AbilityData(_))))
    }

    case UpdateAbility(id, f) => react {
      abilities.modify(as => upsert(id, AbilityData(Ability("N/A", id))).modify(f)(as))
    }
  }

  override def acceptAction: Action => Boolean = {
    case _: AbilityAction => true
    case _ => false
  }
}

object AbilityHandler {
  val initialState: AbilityHandlerState = AbilityHandlerState(new SimpleSet(_.id, Map()))
}

// TODO Count in sp.abilityhandler.AbilityDevice appears to not be doing anything. Is it necessary?
case class AbilityData(
                        ability: Ability,
                        status: AbilityStatus = AbilityStatus.Unavailable,
                        state: Map[ID, SPValue] = Map(),
                        count: Int = 0,
                        result: Option[Map[ID, SPValue]] = None) {
  val name: String = ability.name
  val id: ID = ability.id
  val preCondition: Condition = ability.preCondition
  val started: Condition = ability.started
  val postCondition: Condition = ability.postCondition
  val resetCondition: Condition = ability.resetCondition
  val parameterIDs: List[ID] = ability.parameterIDs
  val resultIDs: List[ID] = ability.resultIDs
  val attributes: SPAttributes = ability.attributes

  def withResult(result: Map[ID, SPValue]): AbilityData = copy(result = Some(result))
}