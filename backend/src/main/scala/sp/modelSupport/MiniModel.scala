package sp.modelSupport

import akka.actor.ActorSystem

import sp.domain.Logic._
import sp.domain._
import sp.domain.logic.{ActionParser, PropositionParser, SOPLogic}

import sp.virtualdevice._
import sp.virtualdevice.APISPVD._


trait CondStuff {
  case class cond(kind: String, guard: String, actions: String*)
  def c(kind: String, guard: String, actions: String*) = cond(kind, guard, actions:_*)
  def parse(c: cond)(idables: List[IDAble]) = {
    val g = if (c.guard.toLowerCase == "true") Some(AlwaysTrue)
    else if (c.guard.toLowerCase == "false") Some(AlwaysFalse)
    else PropositionParser(idables).parseStr(c.guard) match {
      case Right(p) => Some(p)
      case Left(err) => println(s"Parsing failed on condition: $c.guard: $err"); None
    }
    val xs = c.actions.map { action =>
      (ActionParser(idables).parseStr(action) match {
        case Right(a) => Some(a)
        case Left(err) => println(s"Parsing failed on action: $action: $err"); None
      }).get
    }
    Condition(g.get, xs.toList, attributes = SPAttributes("kind"->c.kind))
  }
}

trait Resource extends CondStuff {
  val id = ID.newID
  var abilities: List[Operation] = List.empty
  var things: List[Thing] = List.empty

  def v(name: String, initialState: SPValue): ID = {
    val t = Thing(name, SPAttributes("initialState" -> initialState))
    things = t :: things
    t.id
  }
  def v(name: String): ID = {
    val t = Thing(name)
    things = t :: things
    t.id
  }

  def makeResource(system: ActorSystem): SPResource

  def a(name: String)(conds: cond*): ID = {
    val conditions = conds.toList.map(c=>parse(c)(things))
    val ab = Operation(name, conditions)
    abilities = ab :: abilities
    ab.id
  }

}

trait MiniModel extends CondStuff {
  var resources: Map[String, Resource] = Map.empty
  var operations: List[Operation] = List.empty
  def use(name: String, resource: Resource) = resources = resources + (name -> resource)

  def o(name: String, ab: String)(conds: cond*) = {
    val allAbs = resources.map { case (rn, r) => r.abilities.map(a=>a.copy(name = rn + "." + a.name)) }.flatten.toList
    val ability = allAbs.find(_.name == ab).get

    val parseHelpers = resources.map { case (rn, r) => r.things.map(t=>t.copy(name = rn + "." + t.name)) }.flatten.toList
    val conditions = conds.toList.map(c=>parse(c)(parseHelpers ++ things))

    // merge op with its ability => merging the conditions
    val op = Operation(name, conditions ++ ability.conditions, SPAttributes("ability" -> ab))
    operations = op :: operations
    op.id
  }
  def o(name: String)(conds: cond*) = {
    val parseHelpers = resources.map { case (rn, r) => r.things.map(t=>t.copy(name = rn + "." + t.name)) }.flatten.toList
    val conditions = conds.toList.map(c=>parse(c)(parseHelpers ++ things))

    val op = Operation(name, conditions, SPAttributes())
    operations = op :: operations
    op.id
  }

  var things: List[Thing] = List.empty

  def v(name: String, initialState: SPValue): ID = {
    val t = Thing(name, SPAttributes("initialState" -> initialState))
    things = t :: things
    t.id
  }
  def v(name: String): ID = {
    val t = Thing(name)
    things = t :: things
    t.id
  }

  var sops: List[SOPSpec] = List.empty
  def sop(name: String, sop: List[SOP], attributes: SPAttributes = SPAttributes()) = {
    val s = SOPSpec(name, sop, attributes)
    sops = s :: sops
    s.id
  }

  def getIDAbles(): List[IDAble] = {
    operations ++ things ++ sops ++ resources.map { case (rn, r) => r.things.map(t=>t.copy(name = rn + "." + t.name)) }.flatten.toList
  }

  def makeResources(system: ActorSystem): List[SPResource] = {
    resources.map { case (name, r) => r.makeResource(system) }.toList
  }

  def getInitialState(): State = {
    val varState = things.flatMap(t => t.attributes.get("initialState").map(v => t.id -> v)).toMap
    val opState = operations.map(o => o.id ->
      SPValue(AbilityRunnerTransitions.AbilityStates.notEnabled) // TOOD: fix
    ).toMap

    varState ++ opState
  }

}
