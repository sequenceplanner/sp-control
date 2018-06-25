package spgui.circuits.main.handlers

import diode.{Action, ModelRW}
import monocle.macros.Lenses
import sp.AbilityStatus
import sp.abilityhandler.APIAbilityHandler.Ability
import sp.domain.{Condition, ID, SPAttributes, SPValue}
import spgui.SimpleSet
import spgui.circuits.main.handlers.Aliases.AbilityId


trait AbilityAction extends Action
case class AddAbility(ability: Ability) extends AbilityAction
case class AddAbilities(abilities: List[Ability]) extends AbilityAction
case class UpdateAbility(id: AbilityId, f: AbilityData => AbilityData) extends AbilityAction
case object TerminateAllAbilities extends AbilityAction

@Lenses case class AbilityHandlerState(abilities: SimpleSet[AbilityId, AbilityData])

class AbilityHandler[M](modelRW: ModelRW[M, AbilityHandlerState]) extends StateHandler[M, AbilityHandlerState, AbilityAction](modelRW) {
  import AbilityHandlerState.abilities
  import SimpleSet.upsert

  override def onAction: PartialFunction[AbilityAction, Reaction] = {
    case AddAbility(ability) => react {
      val abilityData = AbilityData(ability)

      (abilities ^|-> upsert(ability.id, abilityData)).modify(_.merge(abilityData))
    }

    case AddAbilities(newAbilities) => react {
      newAbilities.map(AbilityData(_))
          .map(a => (abilities ^|-> upsert(a.id, a)).modify(_.merge(a)))
          .foldLeft(identity[AbilityHandlerState] _)(_ compose _)
    }

    case UpdateAbility(id, f) => react {
      println(s"UpdateAbility $id")
      abilities.modify(as => upsert(id, AbilityData(Ability("N/A", id))).modify(f)(as))
    }

    case TerminateAllAbilities => react {
      println("TerminateAllAbilities")
      abilities.set(SimpleSet[AbilityId, AbilityData](_.id))
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

  /**
    * TODO This function merges state right now. Probably needs to be updated to capture all requirements.
    */
  def merge(other: AbilityData): AbilityData = {
    other.copy(
      state = state ++ other.state
    )
  }
}