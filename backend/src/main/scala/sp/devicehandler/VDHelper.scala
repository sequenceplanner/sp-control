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

  def nn(n: String) = s"$name.$n"
  def unnn(n: String) = n.stripPrefix(s"$name.")

  var dthings: Map[String, Thing] = Map() // driver things
  var things: Map[String, Thing] = Map() // operation things

  var abilities: Map[String, Ability] = Map() // driver abilities
  var operations: Map[String, Operation] = Map() // operations

  var opAbMap: Map[ID,ID] = Map()
  var thingDVThings: Map[ID,ID] = Map()

  var runners: Map[String, APIOperationRunner.Setup] = Map()

  var resources: Map[String, VD.Resource] = Map()
  var drivers: Map[String, VD.Driver] = Map()

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

  def dv(n: String, driverName: String, driverIdentifier: String) = {
    val name = nn(n)
    dthings += (name -> Thing(name, SPAttributes("driverName" -> nn(driverName), "driverIdentifier" -> driverIdentifier)))
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

  def driver(n: String, driverType: String, setup: SPAttributes = SPAttributes()) = {
    val name = nn(n)
    val did = ID.newID

    // below is probably not generally applicable. either move out to model or add
    // identifiers to vd driver api
    val driverIdentifiers = dthings.values.flatMap { v => for {
      driverName <- v.attributes.getAs[String]("driverName") if driverName == name
      driverIdentifier <- v.attributes.getAs[String]("driverIdentifier")
    } yield {
      driverIdentifier
    }}
    val driverSetup = setup merge SPAttributes("identifiers" -> driverIdentifiers)
    val driver = VD.Driver(name, did, driverType, driverSetup)
    drivers += (name -> driver)
  }

  def resource(n: String, things: List[String] = dthings.values.map(_.name).toList,
    attr: SPAttributes = SPAttributes()) = {
    val name = nn(n)
    val checkThings = things ++ things.map(nn) // check both long and short names
    val ids = dthings.filterKeys(dtn=>checkThings.contains(dtn)).values.toList
    val resource = VD.Resource(name, ID.newID, ids.map(_.id).toSet, driverMapper(ids), SPAttributes())
    resources += name -> resource
  }

  def driverMapper(things: List[IDAble]): List[VD.OneToOneMapper] = things.flatMap { v =>
    for {
      driverName <- v.attributes.getAs[String]("driverName")
      driverIdentifier <- v.attributes.getAs[String]("driverIdentifier")
      driver <- drivers.get(driverName)
    } yield {
      VD.OneToOneMapper(v.id, driver.id, driverIdentifier)
    }
  }


}
