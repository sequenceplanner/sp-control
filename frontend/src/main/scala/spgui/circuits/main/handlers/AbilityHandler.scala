package spgui.circuits.main.handlers

import diode.{Action, ModelRW}
import monocle.macros.Lenses
import sp.domain._
import sp.domain.Logic._

import spgui.SimpleSet
import spgui.circuits.main.handlers.Aliases.AbilityId


trait AbilityAction extends Action
case class AddAbility(ability: Operation) extends AbilityAction
case class AddAbilities(abilities: List[Operation]) extends AbilityAction
case class UpdateAbility(id: AbilityId, f: AbilityData => AbilityData) extends AbilityAction
case object TerminateAllAbilities extends AbilityAction

@Lenses case class AbilityHandlerState(abilities: SimpleSet[AbilityId, AbilityData])

// TODO Someone with domain knowledge needs to take a look at how updates happen.
// TODO It is probably incorrect in several places. For example, state might be
// TODO when it should actually be merged, etc.
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
      abilities.modify(as => upsert(id, AbilityData(Operation("N/A", List(), SPAttributes(), id))).modify(f)(as))
    }

    case TerminateAllAbilities => react {
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
                        ability: Operation,
                        status: String = "Unavailable",
                        state: Map[ID, SPValue] = Map(),
                        count: Int = 0,
                        result: Option[Map[ID, SPValue]] = None) {
  val name: String = ability.name
  val id: ID = ability.id
  val nocond = Condition(AlwaysFalse, List())
  def condKind(kind: String) = ability.conditions.find(c=>c.attributes.getAs[String]("kind").contains(kind)).getOrElse(nocond)
  val preCondition: Condition = condKind("pre")
  val started: Condition = condKind("started")
  val postCondition: Condition = condKind("post")
  val resetCondition: Condition = condKind("reset")
  val parameterIDs: List[ID] = ability.attributes.getAs[List[ID]]("parameters").getOrElse(List())
  val resultIDs: List[ID] = ability.attributes.getAs[List[ID]]("results").getOrElse(List()) //TODO not correct/used
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
