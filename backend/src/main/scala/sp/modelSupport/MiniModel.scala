package sp.modelSupport

import akka.actor.ActorSystem
import akka.stream._
import akka.stream.scaladsl._
import akka.{ NotUsed, Done }

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

trait ThingStuff {
  var things: List[Thing] = List.empty


  def v(name: String, initialState: Boolean): ID = {
    vm(name, SPValue(initialState), List(SPValue(false), SPValue(true)), Set())
  }

  def vm(name: String, initialState: Boolean, marked: Set[Boolean]): ID = {
    vm(name, SPValue(initialState), List(SPValue(false), SPValue(true)), marked.map(SPValue(_)))
  }

  def v(name: String, initialState: SPValue, domain: List[SPValue]): ID = {
    vm(name, initialState, domain, Set())
  }

  def vm(name: String, initialState: SPValue, domain: List[SPValue], marked: Set[SPValue]): ID = {
    val t = Thing(name, SPAttributes("initialState" -> initialState, "domain" -> domain, "marked" -> marked))
    things = t :: things
    t.id
  }


  def vu(name: String, initialState: Boolean): ID = {
    vu(name, SPValue(initialState), List(SPValue(false), SPValue(true)))
  }

  def vu(name: String, initialState: SPValue, domain: List[SPValue], marked: Set[SPValue] = Set()): ID = {
    val t = Thing(name, SPAttributes("initialState" -> initialState, "domain" -> domain, "marked" -> marked, "uncontrollable" -> true))
    things = t :: things
    t.id
  }

  // def v(name: String): ID = {
  //   val t = Thing(name)
  //   things = t :: things
  //   t.id
  // }
}

trait Resource extends CondStuff with ThingStuff {
  val id = ID.newID
  var abilities: List[Operation] = List.empty

  def makeResource(system: ActorSystem): SPResource

  def a(name: String)(conds: cond*): ID = {
    val conditions = conds.toList.map(c=>parse(c)(things))
    val ab = Operation(name, conditions)
    abilities = ab :: abilities
    ab.id
  }

  // mappers for drivers

  def stringToIDMapper(mapping: Map[String, ID]) =
    Flow[Map[String, SPValue]].map{ state =>
      mapping.flatMap { case (fieldname, id) => state.get(fieldname).map(spval => id -> spval) }
    }

  def IDToStringMapper(mapping: Map[ID, String]) =
    Flow[Map[ID, SPValue]].map{ state =>
      mapping.flatMap { case (id, fieldname) => state.get(id).map(spval => fieldname -> spval) }
    }

  def mapDomain(mapping: Map[ID, (SPValue => SPValue)]) =
    Flow[State].map{ state =>
      state ++ mapping.flatMap { case (id, f) => state.get(id).map(spval => id -> f(spval)) }
    }

}

trait MiniModel extends CondStuff with ThingStuff {
  var resources: Map[String, Resource] = Map.empty
  var operations: List[Operation] = List.empty
  var specs: List[SPSpec] = List.empty
  def use(name: String, resource: Resource) = resources = resources + (name -> resource)


  def x(name: String, exprs: List[String]) = {
    val parseHelpers = resources.map { case (rn, r) => r.things.map(t=>t.copy(name = rn + "." + t.name)) }.flatten.toList
    val props = exprs.flatMap{x=>
      PropositionParser(parseHelpers ++ things).parseStr(x) match {
        case Right(p) => Some(p)
        case Left(err) => println(s"Parsing failed on forbidden expression: $x: $err"); None
      }
    }
    val spec = SPSpec(name, SPAttributes("forbiddenExpressions" -> props))
    specs = spec :: specs
    spec.id
  }



  def o(name: String, ab: String)(conds: cond*) = {
    val allAbs = resources.map { case (rn, r) => r.abilities.map(a=>a.copy(name = rn + "." + a.name)) }.flatten.toList
    val ability = allAbs.find(_.name == ab).get

    val parseHelpers = resources.map { case (rn, r) => r.things.map(t=>t.copy(name = rn + "." + t.name)) }.flatten.toList
    val conditions = conds.toList.map(c=>parse(c)(parseHelpers ++ things))

    // create operation state variable and conditions for it
    val exec = vm(name + "_exec", false, Set(false)) // mark initial state to find blocking situations
    val isExec = EQ(exec, ValueHolder(SPValue(true)))
    val setExec = List(Action(exec, ValueHolder(SPValue(true))))
    val isNotExec = EQ(exec, ValueHolder(SPValue(false)))
    val resetExec = List(Action(exec, ValueHolder(SPValue(false))))

    val extra1 = List(Condition(isNotExec, setExec, SPAttributes("kind" -> "pre")))
    val extra2 = List(Condition(isExec, resetExec, SPAttributes("kind" -> "post")))


    // merge op with its ability => merging the conditions
    val op = Operation(name, conditions ++ ability.conditions ++ extra1 ++ extra2, SPAttributes("ability" -> ab))
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

  var sops: List[SOPSpec] = List.empty
  def sop(name: String, sop: List[SOP], attributes: SPAttributes = SPAttributes()) = {
    val s = SOPSpec(name, sop, attributes)
    sops = s :: sops
    s.id
  }

  def getIDAbles(): List[IDAble] = {
    operations ++ things ++ sops ++ specs ++ resources.map { case (rn, r) => r.things.map(t=>t.copy(name = rn + "." + t.name)) }.flatten.toList
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
