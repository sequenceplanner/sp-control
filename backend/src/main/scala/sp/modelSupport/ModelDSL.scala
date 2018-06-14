package sp.modelSupport

import sp.abilityhandler.APIAbilityHandler
import sp.abilityhandler.APIAbilityHandler.Ability
import sp.devicehandler._
import sp.domain._
import sp.domain.Logic._
import sp.domain.logic.{ActionParser, PropositionParser}
import sp.runners.APIOperationRunner


case class cond(kind: String, guard: String, actions: String*)
sealed trait ModelElement
case class Tdv(name: String, driverName: String, driverIdentifier: String) extends ModelElement
case class Tv(name: String, initState: SPValue, domain: List[SPValue]) extends ModelElement
case class Ta(name: String, parameters: List[String], pre:cond, running:cond, post:cond, reset:cond=cond("reset", "true")) extends ModelElement
case class To(name: String, pre:cond, post:cond=cond("post", "false"), ab: String = "") extends ModelElement
case class Trunner(name: String, initState: Map[String, SPValue] = Map(), ops: List[String] = List()) extends ModelElement
case class Tresource(name: String, dvs: List[String] = List()) extends ModelElement
case class Tdriver(name: String, driverType: String, setup: SPAttributes = SPAttributes()) extends ModelElement
case class Tsubmodel(name: String, model: List[ModelElement]) extends ModelElement


trait ModelDSL extends BuildModel {
  def buildModel(name: String) = build(name, mes)
  def c(kind: String, guard: String, actions: String*) = cond(kind, guard, actions:_*)
  var mes: List[ModelElement] = List()

  def dv(name: String, driverName: String, driverIdentifier: String) = mes :+= Tdv(name, driverName, driverIdentifier)
  def v(name: String, initState: SPValue, domain: List[SPValue] = List()) = mes :+= Tv(name, initState, domain)
  def a(name: String, parameters: List[String], pre:cond, running:cond, post:cond, reset:cond=cond("reset", "true")) = mes :+= Ta(name, parameters, pre, running, post, reset)
  def o(name: String, pre:cond, post:cond=cond("post", "false"), ab: String = "") = mes :+= To(name, pre, post, ab)

  def runner(name: String, initState: Map[String, SPValue] = Map(), ops: List[String] = List()) = mes :+= Trunner(name, initState, ops)
  def resource(name: String, dvs: List[String] = List()) = mes :+= Tresource(name, dvs)
  def driver(name: String, driverType: String, setup: SPAttributes = SPAttributes()) = mes :+= Tdriver(name, driverType, setup)
  def use(name: String, other: ModelDSL) = mes :+= Tsubmodel(name, other.mes)

}

trait BuildModel {
  def setupToThing(setup : APIOperationRunner.Setup): Thing = {
    Thing(
      name = setup.name,
      id = ID.newID,
      attributes = SPAttributes(
        "name" -> setup.name,
        "runnerID" -> setup.runnerID,
        "ops" -> setup.ops,
        "opAbilityMap" -> setup.opAbilityMap,
        "initialState" -> setup.initialState,
        "variableMap" -> setup.variableMap,
        "abilityParameters" -> setup.abilityParameters.toList
      )
    )
  }

  def driverMapper(drivers: List[VD.Driver], things: List[IDAble]): List[VD.OneToOneMapper] = things.flatMap { v =>
    for {
      driverName <- v.attributes.getAs[String]("driverName")
      driverIdentifier <- v.attributes.getAs[String]("driverIdentifier")
      driver <- drivers.find(_.name == driverName)
    } yield {
      VD.OneToOneMapper(v.id, driver.id, driverIdentifier)
    }
  }

  def parse(c: cond)(idables: List[IDAble]) = {
    val g = if (c.guard.toLowerCase == "true") Some(AlwaysTrue)
    else if (c.guard.toLowerCase == "false") Some(AlwaysFalse)
    else PropositionParser(idables).parseStr(c.guard) match {
      case Right(p) => Some(p)
      case Left(err) => println(s"Parsing failed on condition: $c.guard: $err"); None
    }
    val xs = c.actions.flatMap { action =>
      ActionParser(idables).parseStr(action) match {
        case Right(a) => Some(a)
        case Left(err) => println(s"Parsing failed on action: $action: $err"); None
      }
    }
    Condition(g.get, xs.toList, attributes = SPAttributes("kind"->c.kind))
  }

  case class tempR(name: String, ids: Set[ID], attr: SPAttributes)

  def build(name: String, mes: List[ModelElement]): List[IDAble] = {
    // collect all things and dthings per submodel
    val toplevel = Tsubmodel(name, mes)

    // recursively build submodels
    def buildM(prefix: String, m: Tsubmodel, dvTovMap: Map[ID,ID]=Map(), opAbMap: Map[ID,ID]=Map(), resources: List[tempR] = List()):
        (List[IDAble], Map[ID,ID], Map[ID,ID], List[tempR]) = {
      def nn(n: String) = if(prefix.isEmpty) n else prefix + "." + n
      def unnn(n: String) = if(prefix.isEmpty) n else n.stripPrefix(prefix+".")

      val submodels = m.model.collect { case t: Tsubmodel => t }
      val x = submodels.map(m=>buildM(nn(m.name),m,dvTovMap,opAbMap,resources))
      val deeperThings = x.flatten{_._1}
      val upddvTovMap_ = dvTovMap ++ x.flatten{_._2}.toMap
      val updopAbMap_ = opAbMap ++ x.flatten{_._3}.toMap
      val updResources_ = resources ++ x.flatten{_._4}

      // now we have all the variables "below" this level

      val vs = m.model.collect{
        case Tv(name: String, initState: SPValue, domain: List[SPValue]) =>
          Thing(nn(name), SPAttributes("init" -> initState, "domain" -> domain))
      }
      val dvs = m.model.collect {
        case Tdv(name, driverName, driverIdentifier) =>
          Thing(nn(name), SPAttributes("driverName" -> driverName, "driverIdentifier" -> driverIdentifier))
      }

      // by default create v:s for dv:s if not already defined
      val newVs = dvs.filterNot(dv=>vs.exists(_.name == dv.name)).map(dv=>Thing(dv.name))
      val updVs = vs ++ newVs
      val upddvTovMap: Map[ID,ID] = upddvTovMap_ ++ updVs.flatMap { v =>
        dvs.find(_.name == v.name).map(dv=>v.id->dv.id) } .toMap

      // we can now add abilities and operations matching to the names we have
      val abParseHelpers = dvs ++ dvs.map(dv=> dv.copy(name = unnn(dv.name)))
      val abs = m.model.collect {
        case Ta(name, params, pre, running, post, reset) =>
          val precond = parse(pre)(abParseHelpers)
          val runningcond = parse(running)(abParseHelpers)
          val postcond = parse(post)(abParseHelpers)
          val resetcond = parse(reset)(abParseHelpers)
          val a = Ability(nn(name), ID.newID, precond, runningcond, postcond, resetcond, List())
          APIAbilityHandler.abilityToOperation(a)
      }

      val opParseHelpers = updVs ++ updVs.map(v=> v.copy(name = unnn(v.name)))
      val searchAbs = deeperThings.collect { case o: Operation => APIAbilityHandler.operationToAbility(o) }.flatten
      val opsAndMapping = m.model.collect {
        case To(name, pre, post, ab) =>
          val precond = parse(pre)(opParseHelpers)
          val postcond = parse(post)(opParseHelpers)
          val op = Operation(nn(name), List(precond, postcond))
          val mapping = if(ab.nonEmpty) {
            // hard coded mapping
            val a = searchAbs.find(a=>unnn(a.name)==ab).get // break if we mis-spell
            Some(op.id->a.id)
          } else {
            // find by operation name, must be on the same "level"
            abs.find(_.name==op.name).map(a=>op.id->a.id)
          }
          (op, mapping)
      }

      val ops = opsAndMapping.map{_._1}
      val updOpAbMap = updopAbMap_ ++ opsAndMapping.flatMap{_._2}.toMap

      val levelThings = updVs ++ dvs ++ abs ++ ops

      val updResources = updResources_ ++ m.model.collect {
        case Tresource(name: String, dts: List[String]) =>
          val n = nn(name)
          println("adding resource: " + n)
          val ids = if(dts.isEmpty) {
            // take all dvs on this level
            // TODO: perhaps collect dvs on all sublevels if they dont have a resource
            dvs
          } else {
            // take specified names
            val check = (deeperThings ++ updVs ++ dvs).collect {
              case t: Thing if t.attributes.keys.contains("driverName") && // TODO, maybe add an isa for dvs
                  (dts.contains(t.name) || dts.contains(unnn(t.name))) => t
            }
            assert(check.length == dts.length) // crash if we make a mistake
            check
          }
          tempR(n, ids.map(_.id).toSet, SPAttributes()) // todo: include attributes
      }

      println("deeper short name: ")
      deeperThings.map(t=>unnn(t.name)).foreach {println}
      println("deeper full name")
      deeperThings.map(t=>(t.name)).foreach {println}

      println("level short name: ")
      levelThings.map(t=>unnn(t.name)).foreach {println}
      println("level full name")
      levelThings.map(t=>(t.name)).foreach {println}

      val things = deeperThings ++ levelThings
      (things, upddvTovMap, updOpAbMap, updResources)
    }

    val x = buildM(toplevel.name, toplevel)

    // driver names are global. collect the drivers first, then
    // collect all identifiers from the model
    def getDrivers(l: List[ModelElement]): List[Tdriver] = {
      l.collect {
        case t:Tdriver => List(t)
        case s:Tsubmodel => getDrivers(s.model)
      }.flatten
    }
    val drivers = getDrivers(toplevel.model).map { case Tdriver(name, driverType, setup) =>
      val did = ID.newID

      val thingsToSearch = x._1.collect { case t: Thing => t }
      val driverIdentifiers = thingsToSearch.flatMap { v => for {
        driverName <- v.attributes.getAs[String]("driverName") if driverName == name
        driverIdentifier <- v.attributes.getAs[String]("driverIdentifier")
      } yield {
        driverIdentifier
      }}

      val driverSetup = setup merge SPAttributes("identifiers" -> driverIdentifiers)
      VD.Driver(name, did, driverType, driverSetup)
    }
    val driverThings = drivers.map(d => VD.driverToThing(d))

    println("DRIVERS: " + drivers)

    // now that we have our driver ids, we can set up our resources
    val resources = x._4
    val resourceThings = x._4.map { r =>
      val things = x._1.collect { case t: Thing if r.ids.contains(t.id) => t }
      VD.resourceToThing(
        VD.Resource(r.name, ID.newID, r.ids, driverMapper(drivers, things), SPAttributes()))
    }

    println("RESOURCES: " + resources)

    // TODO: fix this. for now assume that runner is on the top level
    // only and take the entire model
    val r = toplevel.model.collect { case r:Trunner => r }.head
    val dvTovMap = x._2
    val opAbMap = x._3
    val includeOps = x._1.collect { case o: Operation if opAbMap.contains(o.id) => o }.toSet
    val init = x._1.collect { case t: Thing if dvTovMap.values.toList.contains(t.id)=> t }.
      flatMap { t => t.attributes.get("init").map(v=>t.id->v) }.toMap

    val runner = APIOperationRunner.Setup(r.name, ID.newID, includeOps,
      opAbMap, init, dvTovMap, Map()) //TODO add parameters

    val runnerThing = setupToThing(runner)

    x._1 ++ driverThings ++ resourceThings ++ List(runnerThing)
  }
}
