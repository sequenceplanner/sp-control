package sp.devicehandler

import sp.abilityhandler.APIAbilityHandler
import sp.abilityhandler.APIAbilityHandler.Ability
import sp.devicehandler.VD.DriverStateMapper
import sp.devicehandler._
import sp.runners.APIOperationRunner
import sp.domain.Logic._
import sp.domain._
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.vdtesting.APIVDTracker

import scala.concurrent.duration._

trait VDHelper {
  val name: String
  val resource: VD.Resource
  val driver: VD.Driver

  def nn(n: String) = s"$name.$n"
  def unnn(n: String) = n.stripPrefix(s"$name.")

  var dthings: Map[String, Thing] = Map() // driver things
  var things: Map[String, Thing] = Map() // operation things

  var abilities: Map[String, Ability] = Map() // driver abilities
  var operations: Map[String, Operation] = Map() // operations

  var opAbMap: Map[ID,ID] = Map()
  var thingDVThings: Map[ID,ID] = Map()

  var runners: Map[String, APIOperationRunner.Setup] = Map()

  def ac(kind: String, guard: String, actions: String*) = {
    val dt = dthings.values.toList.map(t=>t.copy(name = unnn(t.name)))
    c(kind, guard, actions:_*)(dthings.values.toList++dt)
  }

  def oc(kind: String, guard: String, actions: String*) = {
    val ts = things.values.toList.map(t=>t.copy(name = unnn(t.name)))
    val os = operations.values.toList.map(t=>t.copy(name = unnn(t.name)))
    val all = things.values.toList++operations.values.toList ++ ts ++ os
    c(kind, guard, actions:_*)(all)
  }

  def c(kind: String, guard: String, actions: String*)(idables: List[IDAble]) = {
    // try to parse with short name first, if we fail parse with long name
    val g = if (guard == "true") Some(AlwaysTrue)
    else if (guard == "false") Some(AlwaysFalse)
    else PropositionParser(idables).parseStr(guard) match {
      case Right(p) => Some(p)
      case Left(err) => println(s"Parsing failed on condition: $guard: $err"); None
    }
    val xs = actions.flatMap { action =>
      ActionParser(idables).parseStr(action) match {
        case Right(a) => Some(a)
        case Left(err) => println(s"Parsing failed on action: $action: $err"); None
      }
    }
    Condition(g.get, xs.toList, attributes = SPAttributes("kind"->kind))
  }

  def driverMapper(driverID: ID): Iterable[VD.OneToOneMapper] = dthings.values.flatMap { v =>
    v.attributes.getAs[String]("drivername").map(dn => VD.OneToOneMapper(v.id, driverID, dn))
  }

  def dv(n: String, drivername: String) = {
    val name = nn(n)
    dthings += (name -> Thing(name, SPAttributes("drivername" -> drivername)))
    v(n) // driver things get an operation thing by default
  }

  def v(n: String) = {
    val name = nn(n)
    val thing = Thing(name)
    things += (name -> thing)
    dthings.get(name).foreach { t => thingDVThings += thing.id -> t.id }
  }

  def a(n:String, parameters: List[ID], pre:Condition, exec:Condition, post:Condition, reset:Condition=Condition(AlwaysTrue, List()),pairs: Map[String, SPValue] = Map()) = {
    val name = nn(n)
    val pairsAndName = pairs + ("name" -> SPValue(name))
    abilities += (name -> Ability(name, ID.newID, pre, exec, post, reset, parameters, attributes = SPAttributes("pairs" -> pairsAndName)))
    o(n, oc("pre", "true"), oc("pre", "false"), name) // all abilities get an operation by default
  }

  def o(n:String,pre:Condition,post:Condition=Condition(AlwaysFalse, List()),ab:String = "", attr:SPAttributes=SPAttributes()) = {
    val name = nn(n)
    val op = Operation(name = name, conditions = List(pre, post), attributes = attr)

    // if ability explicitly named, update map
    if(ab.nonEmpty) {
      assert(abilities.keys.exists(_ == ab))
      abilities.get(ab).foreach { a => opAbMap += op.id -> a.id }
    } else {
      // check if abilitymap already exists
      val existingAb = operations.get(name).flatMap(o => opAbMap.get(o.id))
      existingAb.foreach { aid => opAbMap += op.id -> aid }
    }

    operations += (name->op) // update operation if explicitly defined
  }

  def r(n: String, initState: Map[String, SPValue], ops: Iterable[String] = operations.keys) = {
    val name = nn(n)
    val opxs = ops.flatMap(operations.get) ++ ops.map(nn).flatMap(operations.get)

    val init = for {
      (t,v) <- initState ++ initState.map(x=>nn(x._1)->x._2)
      k <- things.get(t)
    } yield {
      k.id -> v
    }

    println("initstate*****: " + init)
    println("var map: " + thingDVThings)
    println("dvs: " + dthings.map(_._2))
    println("things: " + things.map(_._2))

    runners += (name->APIOperationRunner.Setup(
      name = name,
      runnerID = ID.newID,
      ops = opxs.toSet,
      opAbilityMap = opAbMap.filterKeys(s=>opxs.toList.exists(_.id==s)),
      initialState = init,
      variableMap = thingDVThings,
      abilityParameters = Map()
    ))
  }

}
